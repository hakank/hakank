use v6;

say qq/
Problem 4

A palindromic number reads the same both ways. The largest palindrome made 
from the product of two 2-digit numbers is 9009 = 91 × 99.
 
Find the largest palindrome made from the product of two 3-digit numbers.
/;

my $n = 999;
my @x = (1..$n);
my %h = gather for @x X @x {
   my $z=[*] @^x;
   take $z => @^x if $z eq $z.flip;
};

say %h{my $z = [max] %h.keys>>.Int}; 
say $z;

# 2011-03-19: It took 11:55.50min
# 913 993
# 906609
#
# 2019-05-01: It took 7.5s
# (993 913)
# 906609

