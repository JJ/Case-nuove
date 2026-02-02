#!/usr/bin/env perl

use strict;
use warnings;
use v5.14;

my %map = ( Evangeliche => "Lunghi",
            Apostoliche => "Lunghi",
            Vecchie => "Lunghi",
            Ducali => "Curti",
            Nuove => "Curti",
            Nuovissime => "Curti",
            Soldi => "Curti" );


open my $fh, "<", "data-raw/family-types.csv";
open my $out, ">", "data/family-labels.csv";

my $header = <$fh>;
say $out "Family,Group";

while (my $row = <$fh>) {
  chomp($row);
  my ($family,$type) = split(/,\s*/, $row);
  
  if ( $type ne "Unknown" && $type ne "Estinte" ) {
    say $out "$family, ", $map{$type};
  }
}

