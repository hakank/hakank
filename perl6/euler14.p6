use v6;

say qq/
Problem 14

The following iterative sequence is defined for the set of positive integers:

n n\/2 (n is even)
n 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following 
sequence:
13 40 20 10 5 16 8 4 2 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 
10 terms. Although it has not been proved yet (Collatz Problem), it is 
thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.

/;

# Principal approach is using sequences, e.g.
#
# > 11,{($^x %% 2) ?? ($^x / 2) !! (1+ $^x * 3)}...1
# 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1
#

#
# Testing (with output of each sequence):
#   $limit:    100: 20s     (answer: 97, len 119)
#             1000: 6:59min (answer: 871, len 179)
#            10000: -       (answer: 6171 len: 262)
# 
#  With memoizing (and no output of each sequence)
#             100:   10.6s
#            1000: 3:14min
#           10000: 52:58min 
#

my %h = (); # memoize

my $max_n = 0;
my $max_len = 0;
my $limit = 1000000;
for 2...*>$limit -> $n {

  say if $n %% 100;

  # if we seen this number before then it's not interesting
  # (since it can't possible be longer than any earlier 
  # sequence).
  print "X" and next if %h{$n};
  print ".";

  # my $len = (my @c = ($n,{($^x %% 2) ?? ($^x / 2) !! (1 + $^x * 3)}...1)).elems;
  # little nicer:
  my $len = (my @c = ($n,{collatz($^x)}...1)).elems;
  # say "$n: @c[] len: $len";

  # Now loop through all the numbers in the sequence and memoize them
  for @c Z (0...*) -> $k, $i {
    last if %h{$k}; # we have seen this sequence already
    %h{ $k } = $len - $i;
  }

  last if $n > $limit;
  if $len > $max_len {
     $max_len = $len;
     $max_n = $n;
  }
}

say "\n$max_n len: $max_len";


sub collatz($n) {
   return ($n %% 2) ?? ($n / 2) !! (1 + $n * 3);
}