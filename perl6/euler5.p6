use v6;

# Problem 5
say qq/Problem 5: 

2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
/;

# From Twitter...
# This is lcm(1..20)
# say reduce {($^a*$^b/($a,$b,*%*...0)[*-2]).Int},(1..20);

# my own take
sub infix:<gcd>($u, $v) { if $v == 0 { 
                              abs($u) 
                           } else { 
                              $v gcd ($u % $v) 
                           } 
                        };

our sub infix:<lcm>($a, $b) { return $a * $b / ($a gcd $b) };


say [lcm] 1..20;

