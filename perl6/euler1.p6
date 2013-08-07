use v6;
say qq{
Euler Problem 1: 

Find the sum of all the multiples of 3 or 5 below 1000.
};

my $euler1 = [+]((1..999).grep({$_%%(3|5)}));
say $euler1;
# 233168
