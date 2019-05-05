use v6;
use Test;

# Testing the methods in Hakank.pm
#
# Run as
#
# $ perl6 test_hakank.p6
# 


use lib ".";
use Hakank;

my @fib = (1,1,*+*...*)[^10];
my @a = 1..10;

is @fib, <1 1 2 3 5 8 13 21 34 55>, "fib";
is diff(@fib), <0 1 1 2 3 5 8 13 21>, 'diff(@fib)';
is @fib.&diff, <0 1 1 2 3 5 8 13 21>, '@fib.&diff';
is @fib.&diff.&diff, <1 0 1 1 2 3 5 8>, '@fib.&diff.&diff';
is diffn(@fib, 2), <1 0 1 1 2 3 5 8>, 'diffn(@fib, 2)';

is mean(@a), 5.5, 'mean';
is repeat({ @a.&mean}, 5), <5.5 5.5 5.5 5.5 5.5>, "repeat";

my @b = <8 1 7 10 9 8 3 7 10 10>;
is table(@b), {"8" => 2, "1" => 1, "7" => 2, "10" => 3, "9" => 1, "3" => 1}, "table";

is positions(1..20, {$_ %% 3}), <2 5 8 11 14 17>, "positions";
is bool2pos(<0 1 0 1 0 1 0 1 0>), <1 3 5 7>, "bool2pos";
is pos2bool((0..10).map: {$_%%3}), (1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0), "pos2bool";

is compress(<0 1 2 3 4 5>, <1 0 1 1 0 1>), (0,2,3,5), "compress";
is compress("ABCDEF".comb, <1 0 1 1 0 1>), <A C D F>, "compress";
is "ABCDEF".comb /// <1 0 1 1 0 1>, <A C D F>, "compress infix (///)";

is <1 1 1 2 2 13 1 13 1>.&group, {1 => <0 1 2 6 8>, 13 => <5 7>, 2 => <3 4>}, "group";

is head(@a), 1, "head";
is tail(@a), <2 3 4 5 6 7 8 9 10>, "tail";
is butlast(@a), <1 2 3 4 5 6 7 8 9>, "butlast";

is take_while(@a, ( * < 4 ) ), <1 2 3>, "take_while";
is @a.&drop_if(* < 5), <5 6 7 8 9 10>, "drop_if";

# Note: It seems that one can not use "gather" ... "take" as a parameter to "is"...
# is if_do_else(@a, {$_ %% 2}, {$_ / 2 }, { 1  + ($_ * 3) }),
sub test_if_do_else {
    return gather if_do_else(@a, {$_ %% 2}, {take $_ / 2 }, { take 1  + ($_ * 3) });
}
# is if_do_else(@a, {$_ %% 2}, {$_ / 2 }, { 1  + ($_ * 3) }), <4 1 10 2 16 3 22 4 28 5>, "if_do_else";
is test_if_do_else, <4 1 10 2 16 3 22 4 28 5>, "if_do_else";


is 10 gcd 4, 2, "gcd infix";
is 10 lcm 4, 20, "lcm infix";
is [lcm](2..20), 232792560, "[lcm] 2..20";


is is_prime(10), Bool::False, "is_prime(10)";
is is_prime(11), Bool::True, "is_prime(11)";
is  (1 * is_prime($_) for 1..10), <0 1 1 0 1 0 1 0 0 0>, "is_prime seq";

is next_prime(10), 11, "next_prime(10)";
is (2,{next_prime($_)}...*>30), <2 3 5 7 11 13 17 19 23 29 31>, "next_prime seq";
is ([*] primes_slow(100)), 2305567963945518424753102147331756070, "sum primes_slow";

is reduced_digit_sum(777), 3, "reduced_digit_sum(11)";
my %alpha = "a".."z" Z=> (1..26);
is reduced_digit_sum(%alpha{"bach".comb}), 5, "reduced_digit_sum(bach)";

is collatz1(10), 5, "collatz1";
is collatz(10), <10 5 16 8 4 2 1>, "collatz";

is collatz(2**10-1).elems, 63, " collatz(2**10-1).elems";

sub fib_test($a) { return 1 if $a <= 1; return fib_test($a-1) + fib_test($a-2) };
is timeit({fib_test(10)}), 0, "timeit"; 

is tc(<1 2 3 4 0>, 1), <1 2 3 4 0>, "tc";
is tc(<0 2 3 0 1 4>, 5), <5 4 1 2 3 0>, "tc";

is <13 2 0 1 4>.&sort_perm, (2, 3, 1, 4, 0), "grade up (sort_perm1)";
is "hakank kjellerstrand".comb.&sort_perm, <6 1 3 17 19 9 12 0 8 2 5 7 10 11 4 18 13 16 14 15>, "sort_perm2";

is ⍋ <13 2 0 1 4>, <2 3 1 4 0>, "⍋ <13 2 0 1 4> (sort perm infix)";
is ⍋⍋<70 10 30 20>, <3 0 2 1>, "⍋⍋<70 10 30 20> (sort perm sort_perm)";

is each({2*$^x}, 1..10), (2, 4, 6, 8, 10, 12, 14, 16, 18, 20), "each1";
is {2*$^x+1 if $^x %% 2}¨(1..10), (5, 9, 13, 17, 21) , "each infix";

is base([10,10,10,10], <4 1 2 3>), 4123, "base";
is base([7, 24, 60, 60], <0 1 0 0>), 1440, "base (time: seconds per day)";
is base([7, 24, 60, 60], <1 0 0 0>), 86400, "base (time: seconds per week)";


is unbase(<10 10 10 10>, 1234), <1 2 3 4>, "unbase1";
is unbase(<24 60 60>, 12345), <3 25 45>, "unbase2";
is unbase(<24 60 60>, 123456), <1 10 17 36>, "unbase3";
is <10 10 10 10> ⊤ 1234, <1 2 3 4>, "unbase4 (infix)";