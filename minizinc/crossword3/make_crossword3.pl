#!/usr/local/bin/perl
# 
# Fri Sep 23 06:56:31 2011/hakank@bonetmail.com# 
# 
# Reads a crossword problem (grid) from a text file.
# 
$|=1;
use strict;
use warnings;
# use diagnostics;

my $file = $ARGV[0] || die "Syntax: make_crossword3.pl gridfile\n";

open my $fh, "<", $file or die "Cannot open $file: $!";
my $grid = ""; 
while (<$fh>) {
  chomp;
  next if /^\s*[#%]/;
  $grid .= "$_\n";
}


warn "GRID:\n$grid\n";
print crossword2($grid);

#
# Main routines
#
sub crossword2 {
    my ($grid) = @_;

    my ($problem, $max_letters) = make_problem($grid);

    my %crossword = ();
    my %crossword_transposed = ();

    my $r = 0;
    my $c = 0;
    for (split /\n/, $problem) {
        s/(?:^\s+|\s+$)//g;
        my @line = split /[^\d]+/;
        $c = 0;
        for (@line) {
            $crossword{$r}{$c} = $_;
            $crossword_transposed{$c}{$r} = $_;
            $c++;
        }
        $r++;
    }
    my $rows = $r;
    my $cols = $c;
    
    my @segments = ();
    get_segments(\%crossword, $rows, $cols, \@segments);
    get_segments(\%crossword_transposed, $cols, $rows, \@segments);
        
    my @tables = ();
    my $segments_str = "";
    for (@segments) {
        my @segment = @{$_};
        my $len = scalar @segment;
        my $s = join ", ", map {"L[$_]" } @segment;
        push @tables, "table([$s], words$len)";
        $segments_str .= join ",", @segment;
        $segments_str .= join "", ",0" x ($cols - scalar @segment);
        $segments_str .= ",\n";
    }

    my $num_segments = scalar @segments;
    my $table_str = join " \n/\\ ", @tables;
    my $date = scalar localtime;
    my $grid_presentation = join "\n", map {"%   $_"} split /\n/, $grid;
    
    #
    # The MiniZinc model
    #
    return <<EOT;
%
% Random crossword problem in MiniZinc.
%
% (generated $date by `make_crossword3.pl $file`).
%
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank\@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Problem to solve:
% 
$grid_presentation

include "crossword3.mzn"; 

problem_name = "crossword";


% number of letters to assign
N = $max_letters;


% distinct words
require_distinct_words = 1;

constraint
  $table_str
;

% The segments (words)
% (0 for fill outs)
int: max_length = $cols;
int: num_segments = $num_segments;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
$segments_str
]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = $rows;
int: cols = $cols;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
$problem
]);


EOT


} # end crossword



#
# Search for the segments in the grid.
# We define a segment as a sequence of length > 1
# that has number > 0.
#
sub get_segments {
  my ($h, $rows, $cols, $segments, $mode) = @_;

  my $c = 0;
  for my $row (0..$rows-1) {
    my @this_segment = ();
    for my $col (0..$cols-1) {
      my $v = $h->{$row}{$col} || 0;
      if ($v == 0) {
        if (@this_segment > 0 and $c > 0) {
          push @{$segments}, [@this_segment]; 
        }
        @this_segment = (); 
      } else {
        push @this_segment, $v;
      }
      $c++;
    }

    if (@this_segment > 0 and $c > 0) {
      push @{$segments}, [@this_segment];
    }
  }

}

#
# Make the word definitions
#
sub make_words {
  my ($words) = @_;

  my %words = ();
  for (@$words) {
    my @w = split //;
    my $len = scalar @w;
    push @{$words{$len}}, [@w];
  }
  
  my $all_words_str = "";
  for (sort {$a <=> $b} keys %words) {
    my @these_words = @{$words{$_}};
    my $len = $_;
    my $w_str = "";
    for (@these_words) {
      $w_str .= join ",", @$_;
      $w_str .= ",\n";
    }
    my $num = scalar @these_words;
    $all_words_str .= <<EOT;
int: num_words$len = $num;
array[1..num_words$len, 1..$len] of int: words$len = array2d(1..num_words$len, 1..$len,
[
$w_str
]);

EOT

  }

  return $all_words_str;

}


sub make_problem {
  my ($grid) = @_;

  my $id = 1;
  my $problem = "";
  for (split /\n/, $grid) {
    for (split /\s+/) {
      if ("$_" !~ /_/) {
        $problem .= "0, ";
      } else {
        $problem .= "$id, ";
      $id++;
      }
    }
    $problem .= "\n";
  }

  return ($problem, $id-1);

}

