package MIME::Structure;

use strict;

use vars qw($VERSION);

$VERSION = '0.06';

use Text::Balanced qw(extract_delimited);

use constant IN_HEADER   => 1;
use constant IN_BODY     => 2;
use constant IN_PREAMBLE => 3;
use constant IN_EPILOGUE => 4;

use constant PRINT_NONE      => 0;
use constant PRINT_HEADER    => 1;
use constant PRINT_PREAMBLE  => 2;
use constant PRINT_BODY      => 4;
use constant PRINT_EPILOGUE  => 8;

# --- Constructor, accessors, initializer

sub new {
    my $cls = shift;
    my $self = bless {
        'keep_header'    => 0,
        'keep_fields'    => 1,
        'print_header'   => 0,
        'print_preamble' => 0,
        'print_body'     => 0,
        'print_epilogue' => 0,
        @_,
    }, $cls;
    $self->init;
}

sub keep_header { @_ > 1 ? $_[0]->{'keep_header'} = $_[1] : $_[0]->{'keep_header'} }
sub keep_fields { @_ > 1 ? $_[0]->{'keep_fields'} = $_[1] : $_[0]->{'keep_fields'} }
sub print { @_ > 1 ? $_[0]->{'print'} = $_[1] : $_[0]->{'print'} }
sub print_header { @_ > 1 ? $_[0]->{'print_header'} = $_[1] : $_[0]->{'print_header'} }
sub print_body { @_ > 1 ? $_[0]->{'print_body'} = $_[1] : $_[0]->{'print_body'} }
sub print_preamble { @_ > 1 ? $_[0]->{'print_preamble'} = $_[1] : $_[0]->{'print_preamble'} }
sub print_epilogue { @_ > 1 ? $_[0]->{'print_epilogue'} = $_[1] : $_[0]->{'print_epilogue'} }

sub init {
    my ($self) = @_;
    my $print_spec = $self->{'print'};
    my $print;
    if (!defined $print_spec) {
        $print = PRINT_NONE;
    }
    elsif ($print_spec =~ /^\d+$/) {
        $print = $print_spec;
    }
    else {
        if ($print_spec =~ /header/i) {
            $print |= PRINT_HEADER;
        }
        if ($print_spec =~ /body/i) {
            $print |= PRINT_BODY;
        }
        if ($print_spec =~ /preamble/i) {
            $print |= PRINT_PREAMBLE;
        }
        if ($print_spec =~ /epilogue/i) {
            $print |= PRINT_EPILOGUE;
        }
    }
    if ($self->{'print_header'}) {
        $print |= PRINT_HEADER;
    }
    if ($self->{'print_body'}) {
        $print |= PRINT_BODY;
    }
    if ($self->{'print_preamble'}) {
        $print |= PRINT_PREAMBLE;
    }
    if ($self->{'print_epilogue'}) {
        $print |= PRINT_EPILOGUE;
    }
    $self->{'print'} = $print;
    $self;
}

# --- Parsing

sub parse {
    my ($self, $fh) = @_;
    my ($ofs, $line) = (0, 1);
    my $root = $self->{'root'} = {
        'kind'   => 'message',
        'offset' => $ofs,
        'line'   => $line,
        'number' => '1',
    };
    my @entities;
    my @context = ($root);
    my @boundaries;

    # --- Parsing options
    my $keep_header    = $self->keep_header;
    my $keep_fields    = $self->keep_fields;
    my $print          = $self->print;
    my $print_header   = $print & PRINT_HEADER;
    my $print_body     = $print & PRINT_BODY;
    my $print_preamble = $print & PRINT_PREAMBLE;
    my $print_epilogue = $print & PRINT_EPILOGUE;
    
    my $state = IN_HEADER;
    my $header = '';
    while (<$fh>) {
        my $len = length $_;
        $ofs += $len;
        $line++;
        if ($state == IN_HEADER) {
            if (/^$/) {
                # --- Parse the header that has just ended
                print $header, $_ if $print_header;
                my $entity = $context[-1];
                my $fields = $self->parse_header($header);
                my $level = $entity->{'level'} = @context - 1;
                if (@context > 1) {
                    $entity->{'parent'} = $context[-2];
                }
                my ($content_type) = @{ $fields->{'content-type'} || [] };
                if (!defined $content_type) {
                    if (@context >= 2) {
                        my $parent = $context[-2];
                        my $parent_type    = $parent->{'type'};
                        my $parent_subtype = $parent->{'subtype'};
                        if ("$parent_type/$parent_subtype" eq 'multipart/digest') {
                            $content_type = 'message/rfc822';
                        }
                        else {
                            $content_type = 'text/plain; charset=us-ascii';
                        }
                    }
                    else {
                        $content_type = 'text/plain; charset=us-ascii';
                    }
                }
                my ($type, $subtype, $type_params) = parse_content_type($content_type);
                $entity->{'type'}        = $type;
                $entity->{'subtype'}     = $subtype;
                $entity->{'type_params'} = $type_params;
                $entity->{'header'}      = $header if $keep_header;
                $entity->{'fields'}      = $fields if $keep_fields;
                $entity->{'body_offset'} = $ofs;
                $header = '';
                ($entity->{'encoding'})  = map lc, @{ $fields->{'content-transfer-encoding'} ||= ['7bit'] };
                if ($type eq 'multipart') {
                    # --- Header is for a multipart entity
                    $state = IN_PREAMBLE;
                    die "No boundary specified for multipart entity with head er at $ofs"
                        unless defined $type_params->{'boundary'};
                    push @boundaries, $type_params->{'boundary'};
                    $entity->{'parts'} = [];
                }
                else {
                    # --- Header is for a leaf entity
                    $state = IN_BODY;
                    pop @context;  # The entity whose header we just finished reading
                    if ($level == 0 && !($print & PRINT_BODY)) {
                        # Minor optimization: message is not multipart, so we
                        # can stop if we're not going to be printing the body
                        push @entities, $entity;
                        while (<$fh>) { $ofs += length };
                        last;
                    }
                }
                push @entities, $entity;
            }
            else {
                # --- Still in header
                $header .= $_;
            }
        }
        elsif (@boundaries && /^--(.+?)(--)?$/ && $1 eq $boundaries[-1]) {
            print if $print != PRINT_NONE;
            if (defined $2) {
                # End of parent's parts
                pop @boundaries;
                pop @context;
                $state = IN_EPILOGUE;
            }
            else {
                # Another part begins
                $state = IN_HEADER;
                my $part = {
                    'kind'   => 'part',
                    'offset' => $ofs,
                    'line'   => $line,
                };
                my $parent = $context[-1];
                push @{ $parent->{'parts'} }, $part;
                $part->{'parent'} = $parent;
                $part->{'number'} = $parent->{'number'} . '.' . scalar @{ $parent->{'parts'} };
                push @context, $part;
            }
            $header = '';
        }
        elsif ($state == IN_PREAMBLE) {
            # A line within the preamble: ignore per RFC 2049
            print if $print_preamble;
        }
        elsif ($state == IN_EPILOGUE) {
            # A line within the epilogue: ignore per RFC 2049
            print if $print_epilogue;
        }
        else {
            # Normal body line
            print if $print_body;
        }
    }
    $root->{'content_length'} = $ofs - $root->{'body_offset'};
    $root->{'length'} = $ofs;
    
    return @entities;
}

# --- Reporting

sub concise_structure {
    my ($self, $message) = @_;
    # (text/plain:0)
    # (multipart/mixed:0 (text/plain:681) (image/gif:774))
    my $visitor;
    $visitor = sub {
        my ($entity) = @_;
        my $type = $entity->{'type'};
        my $subtype = $entity->{'subtype'};
        my $number = $entity->{'number'};
        my $ofs  = $entity->{'offset'};
        if ($type eq 'multipart') {
            my $str = "($number $type/$subtype:$ofs";
            $str .= ' ' . $visitor->($_) for @{ $entity->{'parts'} };
            return $str . ')';
        }
        else {
            return "($number $type/$subtype:$ofs)";
        }
    };
    $visitor->($message);
}

# --- Utility functions

sub parse_header {
    my ($self, $str) = @_;
    #my $str = $$hdrref;
    $str =~ s/\n(?=[ \t])//g;
    my @fields;
    while ($str =~ /(.+)/g) {
        push @fields, [split /:\s+/, $1, 2];
    }
    return fields2hash(\@fields);
}

sub fields2hash {
    my ($F) = @_;
    my %F;
    foreach (@$F) {
        my ($name, $value) = @$_;
        push @{ $F{lc $name} ||= [] }, $value;
    }
    return \%F;
}

sub parse_content_type {
    my ($str) = @_;
    my ($type, $subtype, $params_str) = split m{/|;\s*}, $str, 3;
    return (lc $type, lc $subtype, parse_params($params_str));
}

sub parse_params {
    my ($str) = @_;
    $str = '' unless defined $str;
    my %param;
    while ($str =~ s/^([^\s=]+)=//) {
        my $name = lc $1;
        if ($str =~ /^"/) {
            my $value = extract_delimited($str, q{"}, '');
            $value =~ s/^"|"$//g;
            $value =~ s/\\(.)|([^\\"]+)|(.)/$+/g;
            $param{$name} = $value;
            # 
        }
        elsif ($str =~ s/^([^\s()<>@,;:\\"\/\[\]?=]+)//) {
            $param{$name} = $1;
        }
        else {
            die "Bad params: $str";
        }
        die "Bad params: $str" unless $str =~ s/^(\s*;\s*|\s*$)//;
    }
    return \%param;
}


1;

=pod

=head1 NAME

MIME::Structure - determine structure of MIME messages

=head1 SYNOPSIS

    use MIME::Structure;
    $parser = MIME::Structure->new;
    $root = $parser->parse($filehandle);
    print $root->{'header'};
    $parts = $root->{'parts'};
    foreach ($parts) {
        $offset  = $_->{'offset'};
        $type    = $_->{'type'};
        $subtype = $_->{'subtype'};
        $line    = $_->{'line'};
        $header  = $_->{'header'};
    }
    print $parser->concise_structure($root), "\n";

=cut

=head1 METHODS

=over 4

=item B<new>

    $parser = MIME::Structure->new;

=item B<parse>

    $root = $parser->parse($filehandle);
    ($root, @other_entities) = $parser->parse($filehandle);

Parses the message found in the given filehandle.

A MIME message takes the form of a non-empty tree, each of whose nodes is
termed an I<entity> (see RFCs 2045-2049).  The root entity is the message
itself; the children of a multipart message are the parts it contains. (A
non-multipart message has no children.)

The B<parse> method returns a list of all the entities in the message; the
first entity is the root entity, the second entity is the root's first child,
and so on.  If called in scalar context, only the root is returned.

Besides parsing the message, this method may also be used to print the message,
or portions thereof, as it parses; see the B<print> method for details.

=item B<keep_header>

    $keep_header = $parser->keep_header;
    $parser->keep_header(1);

Set (or get) whether headers should be remembered during parsing.

=item B<keep_fields>

Set (or get) whether fields (normalized headers) should be remembered.

=item B<print>

    $print = $parser->print;
    $parser->print($MIME::Structure::PRINT_HEADER | $MIME::Structure::PRINT_BODY);
    $parser->print('header,body');

Set (or get) what should be printed.  This may be specified either as any of the
following symbolic constants, ORed together:

=over 4

=item B<PRINT_NONE>

=item B<PRINT_HEADER>

=item B<PRINT_BODY>

=item B<PRINT_PREAMBLE>

=item B<PRINT_EPILOGUE>

=back

Or using the following string constants concatenated using any delimiter:

=over 4

=item B<none>

=item B<header>

=item B<body>

=item B<preamble>

=item B<epilogue>

=back

=item B<print_header>

    $print_header = $parser->print_header;
    $parser->print_header(1);

Set (or get) whether headers should be printed.

=item B<print_body>

    $print_body = $parser->print_body;
    $parser->print_body(1);

Set (or get) whether bodies should be printed.

=item B<print_preamble>

    $print_preamble = $parser->print_preamble;
    $parser->print_preamble(1);

Set (or get) whether preambles should be printed.

=item B<print_epilogue>

    $print_epilogue = $parser->print_epilogue;
    $parser->print_epilogue(1);

Set (or get) whether epilogues should be printed.

=item B<entities>

    $parser->parse;
    print "$_->{type}/$_->{subtype} $_->{offset}\n"
        for @{ $parser->entities };

Returns a reference to an array of all the entities in a message, in the order
in which they occur in the message.  Thus the first entity is always the root
entity, i.e., the message itself).

=item B<concise_structure>

    $parser->parse;
    print $parser->concise_structure;
    # e.g., '(multipart/alternative:0 (text/html:291) (text/plain:9044))'

Returns a string showing the structure of a message, including the content
type and offset of each entity (i.e., the message and [if it's multipart] all
of its parts, recursively).  Each entity is printed in the form:

    "(" content-type ":" byte-offset [ " " parts... ")"

Offsets are B<byte> offsets of the entity's header from the beginning of the
message.  (If B<parse()> was called with an I<offset> parameter, this is added
to the offset of the entity's header.)

N.B.: The first offset is always 0.

=back

=head1 BUGS

Documentation is sketchy.

=head1 AUTHOR

Paul Hoffman E<lt>nkuitse (at) cpan (dot) orgE<gt>

=head1 COPYRIGHT

Copyright 2008 Paul M. Hoffman. All rights reserved.

This program is free software; you can redistribute it
and modify it under the same terms as Perl itself. 

=cut


