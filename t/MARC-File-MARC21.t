# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl MARC-File-MARC21.t'

#########################

use Test::More tests => 5;
use Test::Output;
use File::Slurp;
use Data::Dumper;
stdout_is { print "test" } "test", "Test STDOUT";

BEGIN { use_ok('MARC::File::MARC21') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

sub test {

    my $fileName = shift;
    my $file = MARC::File::MARC21->in( $fileName );
    
    my $encodedDecoded = "";
    while ( my $marc = $file->next() ) {
        $encodedDecoded .= MARC::File::MARC21->encode( $marc );
    }

    my $originalContent = read_file( $fileName );
    ok( $encodedDecoded eq $originalContent, "Content stays same on decode & encode" );
}



test( 't/MARC8.mrc' );
test( 't/UTF8.mrc' );
test( 't/rec.marc' );


