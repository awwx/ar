#!/usr/bin/perl

use strict;

my $expected = '("foo" "bar")';

my $result =
  `./run arc run-test-helper -- foo bar`;

$result eq $expected
    or die "FAIL run-test.pl [$result] instead of [$expected]\n";

print "ok run-test.pl\n";
