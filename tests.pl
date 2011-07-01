#!/usr/bin/perl

use strict;
use File::Temp qw(tempfile);

$ENV{PATH} = '/home/ubuntu/bin:' . $ENV{PATH};

sub which {
    my ($filename) = @_;
    my $result = `which $filename`;
    chomp $result;
    die "$filename not found\n" unless $result;
    $result;
}

my @run_tests = grep(!/root/, split(/\s+/, `ls *.t`));

my @command_tests = (
    ['./arc-script-test.pl'],
    ['sudo', which('mzscheme'), 'run', 'io-root.t'],
    ['sudo', which('racket'),   'run', 'io-root.t']
    );

my @tests = @command_tests;
for my $run_test (@run_tests) {
    # Running the same test file simultaneously is broken because
    # tests use the same temp files and directories.
    push @tests, ['mzscheme', 'run', $run_test];
    push @tests, ['racket',   'run', $run_test];
}

my $children = {};

print "testing...\n";

for my $test (@tests) {
    my ($fh, $filename) = tempfile();
    close $fh;

    my $pid = fork();
    die "can't fork\n" unless defined $pid;
    if ($pid == 0) {
        open(STDOUT, '>', $filename);
        open(STDERR, '>&STDOUT');
        exec(@$test) or die "exec: $!\n";
    }

    $children->{$pid} = [$test, $filename];
}

$| = 1;

while ((my $pid = wait()) != -1) {
    my $child = $children->{$pid};
    next unless defined $child;
    if ($? != 0) {
        system('cat', $child->[1]);
        print "failed: ", join(' ', @{$child->[0]}), "\n";
        kill 'INT', keys(%$children);
        last;
    }
    print "ok: ", join(' ', @{$child->[0]}), "\n";
    delete $children->{$pid};
}
