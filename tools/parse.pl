#!/usr/bin/env perl
#
# This is a simple script to transform README.md into overview.edoc
#
# It has been designed not to be a universal tool to do bidirectional
# transformation between two formats but to satisfy only my needs because
# of a local agreement between developers that README.md should be the
# primary source for documentation, quick-start guide and general
# description. So this parser was born.
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
\@author Serge A. Ribalchenko <fisher\@heim.in.ua>
\@copyright 2013 StrikeAd
\@version $version
EOS

my ($prev_line_is_empty, $is_empty, $unnumbered_list, $numbered_list);

# first pass. Determine structure, grab section names
while (my $line = <IN>) {
  chomp $line;

  $prev_line_is_empty = $is_empty;
  unless ($line =~ /^$/) { $is_empty = 0 }
  else {
    if ( $unnumbered_list ) {
      $unnumbered_list = 0;
      push @out, "</ul>";
    } elsif ( $numbered_list ) {
      $numbered_list = 0;
      push @out, "</ol>";
    } else {
      $is_empty = 1;
    }
    next;
  };

  # headings, sections
  $line =~ s/^### (.*)$/=== $1 ===/;
  $line =~ s/^## (.*)$/== $1 ==/ && push @{$headings{'h2'}}, $1;
  $line =~ s/^# (.*)$/= $1 =/ && push @{$headings{'h1'}}, $1;
  # URL transformation
  $line =~ s{\[(.*)\]\((http|ftp|https)://([^ )]*)\)}{[$2://$3 $1]}g;
  # bold and italic
  $line =~ s{\*\*\*(.*?)\*\*\*}{<b><i>$1</i><\/b>}g;
  $line =~ s{\*\*(.*?)\*\*}{<b>$1<\/b>}g;
  $line =~ s{\*(.*?)\*}{<i>$1<\/i>}g;
  # lists
  $line =~ s/^ \* (.*)$/<li>$1<\/li>/ && do {
    $unnumbered_list = 1;
    if ( $prev_line_is_empty ) { push @out, "<ul>"; } };
  $line =~ s/^ [\d{1,2}]\. (.*)$/<li>$1<\/li>/ && do {
    $numbered_list = 1;
    if ( $prev_line_is_empty ) { push @out, "<ol>"; } };

  push @out, $line;
}

close IN;

# in case we have only one h1 heading assume it is a short title
if (scalar @{$headings{'h1'}} == 1) {
  print OUT "\@title $headings{h1}[0]\n";
  @out = grep (!/$headings{h1}[0]/, @out);
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
