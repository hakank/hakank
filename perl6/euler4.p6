use v6;

say qq/
Problem 4

A palindromic number reads the same both ways. The largest palindrome made 
from the product of two 2-digit numbers is 9009 = 91 × 99.
 
Find the largest palindrome made from the product of two 3-digit numbers.
/;

my $n = 999;
my @n_rev = (1..$n).reverse;

my %h = gather for (@n_rev X @n_rev) {
                    my $z=$^x*$^y; 
                    take ($z => [$^x,$^y]) if $z eq $z.flip
        };

# Not so nice version...
# my %h = gather for ([$n,$n-1...1] X [$n,$n-1...1]) {
#                                     my $z=$^x*$^y; take ($z => [$^x,$^y]) if $z eq $z.flip 
#        };

# say $_ for %h;
# say %h{my $z = [max] %h.keys>>.Int}; 
say %h{my $z = [max] %h.keys>>.Int}; 
say $z;

# 2011-03-19: It took 11:55.50min
# 913 993
# 906609

# alternative version (don't shows the terms, though)
# say [max] gather for ([$n,$n-1...1] X [$n,$n-1...1]) {my $z=$^x*$^y; take ([$^x,$^y]) if $z eq $z.flip };