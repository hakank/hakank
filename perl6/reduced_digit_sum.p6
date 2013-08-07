use v6;

#
# Reduced digit sums for a number or a list of numbers.
# E.g. 777 -> 7+7+7 -> 21 -> 2+1 -> 3
#

say "777: ", reduced-digit-sum(777);


my %alpha = (my @z = ("a".."z","å","ä","ö"," ")) Z=> (1..@z.elems);

my @a = %alpha{"håkan kjellerstrand".split("")};
say @a.perl;
say reduced-digit-sum(%alpha{"håkan kjellerstrand".split("")});

sub reduced-digit-sum($x) { (([+] $x.comb),{[+] .comb}...*<10)[*-1] }