use v6;

say qq/
n! means n (n 1) ... 3 2 1

Find the sum of the digits in the number 100!
/;

# Again, Rakudo Perl6 don't have arbitrary precision so I 
# cheat a little now.

sub postfix:<!> { [*] 1..$^a }

say "17!: {17!}";
say [+] (17!).comb;

say "100!: {100!}";
say [+] (100!).comb;