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
say $out "Family,Group,Accession,Ducale";

open my $fh3, "<", "data/ducali.csv";
my %ducali_families;
map { chomp; $ducali_families{$_} = 1 } <$fh3>;
close $fh3;

while (my $row = <$fh2>) {
  chomp($row);
  my ($family,$date) = split(/,\s*/, $row);
  my $std_family = join( " ", map { ucfirst($_) } split(/\s+/,  lc($family) ) );
  say $std_family;
  my $ducale = $ducali_families{$std_family}?1:0;
  if ( $labels{$std_family} ) {
    say $out "$std_family,$labels{$std_family},$date,$ducale";
  } else {
    say $out "$std_family,Curti,$date,$ducale";
  }
}

close $fh;
close $fh2;
close $out;

