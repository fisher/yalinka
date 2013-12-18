#!/usr/bin/env perl
#
# helper tool to form an overview.edoc from github's README.md
#
# (c) 2013, Serge A. Ribalchenko <fisher@heim.in.ua>
#

use strict;
use warnings;

my @out;
my %headings;
my $sections;

if ($#ARGV != 2) { die "usage: $0 (template file) (target file) (version)" };

my $version = $ARGV[2];

# template file
open IN, "<".$ARGV[0] or die "cunt open file ".$ARGV[0]." for reading: ".$!;

# destination file
open OUT, ">".$ARGV[1] or die "cunt open file ".$ARGV[1]." for writing: ".$!;

# should be exactly three decimal numbers separated by dots
$version =~ m/(\d+\.){2}\d+/ or die "unrecognized version string, \"".$version."\"";

print OUT <<EOS;
\@copyright 2013 Serge A. Ribalchenko, <fisher\@heim.in.ua>
\@version $version
EOS

# first pass. Determine structure, grab section names
while (my $line = <IN>) {
  chomp $line;
  $line =~ s/^### (.*)$/=== $1 ===/;
  $line =~ s/^## (.*)$/== $1 ==/ && push @{$headings{'h2'}}, $1;
  $line =~ s/^# (.*)$/= $1 =/ && push @{$headings{'h1'}}, $1;

  push @out, $line;
}

close IN;

# in case we have only one h1 heading assume it is a short title
if (scalar @{$headings{'h1'}} == 1) {
  print OUT "\@title $headings{h1}[0]\n";
  $sections = 'h2';
} elsif (scalar @{$headings{'h1'}} == 0) {
  print OUT "\@title Untitled project\n";
  $sections = 'h2';
} else {
  print OUT "\@title Untitled project\n";
  $sections = 'h1';
}


# form contents list from high-level headings

print OUT "\@doc == Contents ==\n\n<ol>\n";

for (@{$headings{$sections}}) { print OUT "  <li>{\@section $_}</li>\n"; }

print OUT "</ol>\n\n";


# dump the preparsed lines

for (@out) {print OUT $_."\n";}

close OUT;
