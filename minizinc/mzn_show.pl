#!/usr/bin/perl -w
#
# Since version 1.0 MiniZinc's FlatZinc specification don't support 
# the output [] anymore (except for the minizinc solver), and it can 
# be hard to see structured output, e.g. matrices.
#
# This program show the result in a somewhat nicer output.
#
# Usage:
#   solver problem.mzn | mzn_show.pl "{parameter} ({parameter})"
#
# See below for the explanations of the parameters.
#
# I blogged about the first version of this program here:
# http://www.hakank.org/constraint_programming_blog/2009/06/miscellaneous_news_1.html
#
# This program was created by Hakan Kjellerstrand, hakank@bonetmail.com .
# Also see my MiniZinc page: http://www.hakank.org/minizinc/
#
$|=1;
use strict;
use warnings;

my $args = $ARGV[0] || "";

if ($args =~ /help/i) {
    die "Syntax: flatzinc model.fzn | mzn_show.pl '{parameter} ({parameter})'\n";
}

#
# Explanations of the parameters/options:
#
# Syntax:
# - translate:
#     {tr:from:to:}
#     e.g.:  
#     flatzinc nonogram_regular.fzn | mzn_show.pl "{tr:1: :} {tr:2:#:}"
#
# - translate for just one variable: 
#    {trvar:variable:from:to:}
#     e.g.:  
#   flatzinc nonogram_regular.fzn | mzn_show.pl "{trvar:x:1: :} {trvar:x:2:#:}"
#
# - translate "tr" style (i.e. the Unix command tr)
#   {trtr:string_of_integers:string_of_replacements:}
#   e.g.
#    flatzinc nonogram_regular.fzn | mzn_show.pl "{trtr:12: #:}"
#   
#   Note: Here we assume that both strings are of the same length.
#
# - translate tr style (i.e. the Unix command tr) for just one variable
#   {trtrvar:string_of_integers:string_of_replacements:}
#   e.g.
#    flatzinc nonogram_regular.fzn | mzn_show.pl "{trtrvar:x:12: #:}"
#
#   Note: Here we assume that both strings are of the same length.
#
# - {no_space} or {nospace}
#   Don't print spaces after a digit/character.
#
# - {no_printf} or {noprintf}
#   Don't use the printf stuff to beautify the output.
#
# - {skipresult} or {noresult}
#   Skips the number version of the result, just show the translated result.
#   Only for 2d and 3d arrays.
# 
# - {nonogram}
#   short cut for
#    {trtr:12: #} {nospace} {noresult}   
#
# - {letters:variable}
#
# Note: Using ":" as a replacement character is not a good idea...
#
#  
#
my %translate = ();
my %translate_var = ();
my %translate_tr = ();
my %translate_tr_var = ();
my %translate_letters = ();
my $no_space = 0;
my $no_printf = 0;
my $skip_result = 0;

# For translate_letters
my %letters = ();
my $letter_count = 1;
for (("a".."z","å","ä","ö")) {
   $letters{$letter_count} = $_;
   $letter_count++;
}


if ($args) {
  while ($args =~ /{(.+?)}/g) {
    my $a = $1;

    if ($a =~ m!^tr:(.*?):(.*?):!i) {
        $translate{"$1"} = "$2";

    } elsif ($a =~ m!^tr_?var:(.+?):(.*?):(.*?):!i) {
        $translate_var{"$1\0$2"} = "$3";

    } elsif ($a =~ m!^letters:(.+)\b!i) {
        $translate_letters{"$1"} = 1;
    } elsif ($a =~ m!^tr_?tr:(.+?):(.+?):!i) {
        my @from = split //, $1;
        my @to = split //, $2;
        # Assume that from and to has the same length
        for (my $i = 0; $i < @from; $i++) {
            $translate_tr{"$from[$i]"} = $to[$i];
        }

    } elsif ($a =~ m!^tr_?tr_?var:(.+?):(.+?):(.+?):!i) {
        my $var = $1;
        my @from = split //, $2;
        my @to = split //, $3;
        # Assume that from and to has the same length
        for (my $i = 0; $i < @from; $i++) {
            $translate_tr_var{"$var\0$from[$i]"} = $to[$i];
        }
    } elsif ($a =~ /no_?space/i) {
        $no_space = 1;

    } elsif ($a =~ /no_?printf?/i) {
        $no_printf = 1;
    } elsif ($a =~ /(skip|no)_?result/i) {
        $skip_result = 1;
    } elsif ($a =~/nonogram/i) {
        $translate_tr{"1"} = " ";
        $translate_tr{"2"} = "#";
        $no_space = 1;
        $skip_result = 1;
    }

  }
}

while (<STDIN>) {
  chomp;
  my $line = $_;

  if ($line =~ m!^(.+?) = array2d\((-?\d+)\.\.(-?\d+), (-?\d+)\.\.(-?\d+), \[(.+)\]\);$!) {
    #
    # 2D
    #
    my $what  = $1;
    my $from1 = $2;
    my $to1   = $3;

    my $from2 = $4;
    my $to2   = $5;

    my $array = $6;
    my @a = split /,\s*/, $array;

    my $max_len = get_max_len(@a);
    print "% $line\n" if !$skip_result;
    print "$what:\n" if !$skip_result;

    # normalize indices
    my $to1_tmp = $to1 - $from1;
    my $to2_tmp = $to2 - $from2;
    for my $i (0..$to1_tmp) {
      for my $j (0..$to2_tmp) {
        my $ix = $i*($to2_tmp+1)+$j;
        print_item($a[$ix],$what, $max_len, \%translate, \%translate_var, \%translate_tr, \%translate_tr_var, \%translate_letters, $no_space, $no_printf);
      }
      print "\n";
    }

  } elsif ($line =~ m!^(.+?) = array3d\((-?\d+)\.\.(-?\d+),\s*(-?\d+)\.\.(-?\d+),\s*(-?\d+)\.\.(-?\d+),\s*\[(.+)\]\);$!) {
    #
    # 3D
    #
    my $what  = $1;
    my $from1 = $2;
    my $to1   = $3;

    my $from2 = $4;
    my $to2   = $5;

    my $from3 = $6;
    my $to3   = $7;

    my $array = $8;
    my @a = split /,\s*/, $array;

    my $max_len = get_max_len(@a);
    print "% $line\n" if !$skip_result;
    print "$what:\n" if !$skip_result;

    # normalize indices
    my $to1_tmp = $to1 - $from1;
    my $to2_tmp = $to2 - $from2;
    my $to3_tmp = $to3 - $from3;
    for my $i (0..$to1_tmp) {
      for my $j (0..$to2_tmp) {
          print " ";
          for my $k (0..$to3_tmp) {
            my $ix = $i*(($to2_tmp+1)*($to3_tmp+1))+
                     $j*($to3_tmp+1)+
                     $k;
            print_item($a[$ix], $what, $max_len, \%translate, \%translate_var, \%translate_tr, \%translate_tr_var, \%translate_letters, $no_space, $no_printf);
          }
          print "\n";
      }
      print "\n";
    }

  } elsif ($line =~ m!^(.+?) = array1d\((-?\d+)\.\.(-?\d+), \[(.+)\]\);$!) {
    #
    # Flat arrays
    #
    my $what  = $1;
    my $from1 = $2;
    my $to1   = $3;

    my $array = $4;
    my @a = split /,\s*/, $array;

    # print "% $line\n";
    for (@a) {
        $_ = $translate{$_} if defined $translate{"$_"};
        $_ = $translate_var{"$what\0$_"} if defined $translate_var{"$what\0$_"};
        $_ = $translate_tr{"$_"} if defined $translate_tr{"$_"};
        $_ = $translate_tr_var{"$what\0$_"} if defined $translate_tr_var{"$what\0$_"};
        $_ = $letters{$_} if defined $translate_letters{"$what"};
    }
    print "$what: @a\n";

  } else {
    #
    # Otherwise just print the line.
    #
    print "$line\n";
  }

}


sub get_max_len {
    my (@a) = @_;

    my $max_len = 0;
    for (@a) {
      my $len = length $_;
      $max_len = $len if $len > $max_len;
    }

    return $max_len;
}



sub print_item {
    my ($t, 
        $what, 
        $max_len, 
        $translate, 
        $translate_var, 
        $translate_tr, 
        $translate_tr_var, 
        $translate_letters, 
        $no_space, 
        $no_printf) = @_;

    $t = $translate->{$t} if defined $translate->{"$t"};
    $t = $translate_var->{"$what\0$t"} if defined $translate_var->{"$what\0$t"};
    $t = $translate_tr->{$t} if defined $translate_tr->{"$t"};
    $t = $translate_tr_var->{"$what\0$t"} if defined $translate_tr_var->{"$what\0$t"};
    $t = $letters{$t} if defined $translate_letters->{"$what"};
    if (!$no_printf) {
        printf "%${max_len}s", "$t";
    } else {
        print "$t";
    }
    print " " if !$no_space;
    
}
