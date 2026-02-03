#!/usr/bin/env perl

use strict;
use warnings;
use v5.14;

open my $fh, "<", "data/family-labels.csv";
my %labels;
my $header = <$fh>;

while (my $row = <$fh>) {
  chomp($row);
  my ($family,$type) = split(/,\s*/, $row);
  $labels{$family} = $type;
}

open my $fh2, "<", "data-raw/family-accession-date.csv";
my $header2 = <$fh2>;
open my $out, ">", "data/family-labels-accession.csv";
say $out "Family,Group,Accession";

while (my $row = <$fh2>) {
  chomp($row);
  my ($family,$date) = split(/,\s*/, $row);
  my $std_family = ucfirst( lc($family) );
  if ( $labels{$std_family} ) {
    say $out "$std_family,$labels{$std_family},$date";
  } else {
    say $out "$std_family,Curti,$date";
  }
}

close $fh;
close $fh2;
close $out;

