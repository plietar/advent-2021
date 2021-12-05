#!/usr/bin/env perl
use List::Util qw(min max);
use strict;

my %CELLS;
while (<>) {
  /^(\d+),(\d+) -> (\d+),(\d+)$/;
  if ($1 == $3 || $2 == $4) {
    for my $x (min($1,$3)..max($1,$3)) {
      for my $y (min($2,$4)..max($2,$4)) {
        $CELLS{int($x)}{int($y)} += 1;
      }
    }
  }
}

my $count;
foreach my $x (keys %CELLS) {
  foreach my $y (keys %{%CELLS{$x}}) {
    if ($CELLS{$x}{$y} > 1) {
      $count += 1;
    }
  }
}
print "$count\n";
