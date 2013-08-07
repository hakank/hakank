#!/usr/local/bin/perl
# 
# Mon Sep  7 20:18:05 2009/hakank@bonetmail.com 
# 
# Makes finite automata for nonogram_regular.mzn
# given a data file with the following format
# (this is for the problem instance 'Hen').
# Note that the variable names must be exactly 
# these.
#
#   rows = 9;
#   row_rule_len = 2;
#   row_rules = array2d(1..rows, 1..row_rule_len,
#       [0,3, 
#        2,1, 
#        3,2, 
#        2,2, 
#        0,6, 
#        1,5, 
#        0,6, 
#        0,1, 
#        0,2]);
#
#   cols = 8;
#   col_rule_len = 2;
#   col_rules = array2d(1..cols, 1..col_rule_len,
#       [1,2,
#        3,1, 
#        1,5, 
#        7,1, 
#        0,5, 
#        0,3, 
#        0,4, 
#        0,3]);
#
# This program was created by Hakan Kjellerstrand
# (hakank@bonetmail.com).
# Also see my MiniZinc page: http://www.hakank.org/minizinc/
# 
$|=1;
use strict;
use warnings;
use locale;

my $file = $ARGV[0] || "nonogram_n4.dzn";
my $debug = $ARGV[1] || 0;

open FILE, $file or die "Cannot open $file: $!";
my $all = "";
while (<FILE>) {
    next if /^\s*$/;
    print "$_" and next if /^\s*%/;
    $all .= $_;
}
close FILE;

#
# Extract info
#
my $rows         = $1 if $all =~ /^\s*rows\s*=\s*(\d+)/sm;
my $row_rule_len = $1 if $all =~ /^\s*row_rule_len\s*=\s*(\d+)/sm;
my $row_rules    = $1 if $all =~ /^\s*row_rules\s*=\s*.+?\[(.+?)\]/sm;

my $cols         = $1 if $all =~ /^\s*cols\s*=\s*(\d+)/sm;
my $col_rule_len = $1 if $all =~ /^\s*col_rule_len\s*=\s*(\d+)/sm;
my $col_rules    = $1 if $all =~ /^\s*col_rules\s*=\s*.+?\[(.+?)\]/sm;


warn<<EOT if $debug;
rows: $rows
row_rule_len: $row_rule_len
row_rules: $row_rules

cols: $cols
col_rule_len: $col_rule_len
col_rules: $col_rules

Create automata...

EOT


#
# First handle the row patterns
#
print "%\n% ROW RULES\n%\n";
print <<EOT;
row_max = $rows;
row_states = array2d(1..row_total_states, 1..2, [
EOT

my ($row_num_patterns, $row_max_state, $row_total_states, $row_num_states, 
    $row_start_where, $row_num_states_str, $row_start_where_str) = make_rules($row_rules);

my @row_num_states = @{$row_num_states};
my @row_start_where = @{$row_start_where};


print<<EOT;
]);

row_max_state = $row_max_state;
row_total_states = $row_total_states;
row_num_patterns = $row_num_patterns;

row_num_states = [$row_num_states_str]; % this is also the final state
row_start_where = [$row_start_where_str];

EOT


#
# Then the column patterns
#
print<<EOT;
%
% COL_RULES:
%

col_max = $cols;

col_states = array2d(1..col_total_states, 1..2, [
EOT

my ($col_num_patterns, $col_max_state, $col_total_states, $col_num_states, 
    $col_start_where, $col_num_states_str, $col_start_where_str) = make_rules($col_rules);

my @col_num_states = @{$col_num_states};
my @col_start_where = @{$col_start_where};


print<<EOT;
]);

col_num_patterns = $col_num_patterns;
col_max_state = $col_max_state;
col_total_states = $col_total_states;

col_num_states = [$col_num_states_str]; % this is also the final state
col_start_where = [$col_start_where_str];


EOT


=comment
From nonogram_regular.co:
The states for pattern: rules_tmp[2,1]
int: num_states = 5;
array[1..num_states,1..2] of 0..num_states: states =
array2d(1..num_states, 1..2, [
1, 2,
0, 3,
4, 0,
4, 5,
5, 0,
]);
=cut
#
# create an automaton for a rule
#
sub create_automaton {
  my ($rule) = @_;

  my @rule = split /\s*,\s*/, $rule;
  my $r_len = 0;
  for (@rule) {
    $r_len++ if $_ > 0;
  }
  
  my @rules_tmp = ();
  for (@rule) {
    push @rules_tmp, $_ if $_ > 0;
  }

  my ($automaton, $num_states) = make_transition_matrix(@rules_tmp);

  return ($automaton, $num_states);

}

#
# Make the transition matrix (states)
#
sub make_transition_matrix {
  my (@pattern) = @_;

  my $p_len = scalar @pattern;
  my $num_states = $p_len;
  for (@pattern) {
    $num_states += $_;
  }

  # convert pattern to a 0/1 pattern for easy handling of
  # the states
  my @tmp = ("dummy", 0);
  my $c = 0;
  for(@pattern) {
    for(0..$_-1) {
      push @tmp, 1;
      $c++;
    }
    if ($c < $num_states-2) {
      push @tmp, 0;
    }
  }

  print "% tmp: @tmp\n" if $debug;

  # create the transition matrix
  # int t_matrix[1..num_states, 1..2];
  my @t_matrix = ();
  $t_matrix[0][0] = "dummy";
  $t_matrix[0][1] = "dummy";

  $t_matrix[$num_states][0] = $num_states;
  $t_matrix[$num_states][1] = 0;
  for(my $i = 1; $i <= $num_states; $i++) {

    if ($tmp[$i] == 0) {
      $t_matrix[$i][0] = $i;
      $t_matrix[$i][1] = $i+1;
    } else {
      if ($i < $num_states) {
        if ($tmp[$i+1] == 1) {
          $t_matrix[$i][0] = 0;
          $t_matrix[$i][1] = $i+1;
        } else {
        $t_matrix[$i][0] = $i+1;
        $t_matrix[$i][1] = 0;
        }
      }
    }
    
  }

  my $automaton = "";
  for my $i (1..$num_states) {
    for (@{$t_matrix[$i]}) {
      $automaton .= "$_,";
    }
    $automaton .= "\n";
    
  }

  return ($automaton, $num_states);
}


#
# All rows or column rules
#
sub make_rules {
    my ($rules) = @_;

    my $num_patterns = 0;
    my $max_state = 0;
    my $total_states = 0;
    my @num_states = ();
    my @start_where = (1);
    my $start_where = 1;
    for (split /\n/, $rules) {
        next if $_ =~ /^\s*$/;
        s/^\s+//;
        s/\s+$//;
        my $rule = $_;
        print "% pattern $rule\n";
        my ($automaton, $num_states) = create_automaton($rule);
        print "$automaton\n";
        push @num_states, $num_states;
        $start_where += $num_states;
        push @start_where, $start_where;
        $max_state = $num_states if $num_states > $max_state;
        $total_states += $num_states;
        $num_patterns++;
    }
    
    my $num_states_str = join ",", @num_states;
    
    pop @start_where; # remove last item
    my $start_where_str = join ",", @start_where;

    return ($num_patterns, $max_state, $total_states, \@num_states, \@start_where,
        $num_states_str, $start_where_str);

}
