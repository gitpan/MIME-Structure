use Test::More;

my %message_structure = (
    'simple'    => '(text/plain:0)',
    'one-level' => '(multipart/alternative:0 (text/plain:250) (text/plain:387))',
    'nested'    => '(multipart/mixed:0 (text/plain:276) (multipart/alternative:382 (text/plain:519) (text/plain:661)) (text/plain:767))',
);

plan 'tests' => 2 + 3 * scalar(keys %message_structure);

use_ok( 'MIME::Structure' );

my $parser = MIME::Structure->new;

isa_ok( $parser, 'MIME::Structure' );

foreach my $m (sort keys %message_structure) {
    my $fh;
    ok( open($fh, '<', "t/messages/$m.txt"), "open $m message" );
    ok( $parser->parse($fh), "parse $m message" );
    is( $parser->concise_structure, $message_structure{$m}, "structure of $m message" );
    close $fh;
}
