# -*- perl -*-

# t/001_load.t - check module loading and create testing directory

use Test::More tests => 1;

# Trick ApacheHandler into loading without exploding
sub Apache::perl_hook { 1 }
sub Apache::server { 0 }

BEGIN
{
    use_ok( 'MasonX::WebApp' );
}


