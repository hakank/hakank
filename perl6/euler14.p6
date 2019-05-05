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

# Now it's down to 6min 33.09seconds... To be continued....


# Principal approach is using sequences, e.g.
#
# > 11,{($^x %% 2) ?? ($^x / 2) !! (1+ $^x * 3)}...1
# 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1
#

my $max_n = 0;
my $max_len = 0;
my $limit = 1_000_000;
my %h = Hash.new; # memoize
for 2 .. $limit -> int $n {

  # if we seen this number before then it's not interesting
  # (since it can't possible be longer than any earlier 
  # sequence).
  next if %h{$n};

  say $n if $n %% 10000;

  my $len = 0;
  for ($n,{collatz($^x)}...1) -> $c {
    $len++;  
    if %h{$c} {
      $len = %h{$c} + $len - 1;
      last;
    }
  }  
  %h{$n} = $len;

  if $len > $max_len {
     $max_len = $len;
     $max_n = $n;
     # say "so far: $max_n len: $max_len";
  }
}

say "\n$max_n len: $max_len";


sub collatz($n) {
  return ($n %% 2) ?? ($n / 2) !! (1 + $n * 3);
}
