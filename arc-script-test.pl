#!/usr/bin/perl

use strict;
use FindBin;

my $arcdir = $FindBin::Bin;

my $expected = "(foo bar)\n";

my $result =
  `cd /tmp;
   export PATH=$arcdir:\$PATH;
   $arcdir/test-script/echo foo bar`;

$result eq $expected
    or die "FAIL arc-script-test [$result] instead of [$expected]\n";

print "ok arc-script-test\n";
