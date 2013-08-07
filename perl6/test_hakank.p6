use v6;
use Test;

use Hakank;

my @fib = (1,1,*+*...*)[^10];
say @fib.perl;

is diff(@fib), <0 1 1 2 3 5 8 13 21>, 'diff(@fib)';
is @fib.&diff, <0 1 1 2 3 5 8 13 21>, '@fib.&diff';
is @fib.&diff.&diff, <1 0 1 1 2 3 5 8>, '@fib.&diff.&diff';
is diffn(@fib, 2), <1 0 1 1 2 3 5 8>, 'diffn(@fib, 2)';


my @a = 1..10;

is mean(@a), 5.5, 'mean';
is repeat({ @a.&mean}, 5), <5.5 5.5 5.5 5.5 5.5>, "repeat";

my @b = <8 1 7 10 9 8 3 7 10 10>;
is table(@b), {"8" => 2, "1" => 1, "7" => 2, "10" => 3, "9" => 1, "3" => 1}, "table";

is positions(1..20, {$_ %% 3}), <2 5 8 11 14 17>, "positions";

is head(@a), 1, "head";
is tail(@a), <2 3 4 5 6 7 8 9 10>, "tail";
is butlast(@a), <1 2 3 4 5 6 7 8 9>, "butlast";

is take_while(@a, ( * < 4 ) ), <1 2 3>, "take_while";
is @a.&drop_if(* < 5), <5 6 7 8 9 10>, "drop_if";
is if_do_else(@a, {$_ %% 2}, {$_ / 2 }, { 1  + ($_ * 3) }), 
   <4 1 10 2 16 3 22 4 28 5>, "if_do_else";

is 10 gcd 4, 2, "gcd infix";
is 10 lcm 4, 20, "lcm infix";

is is_prime(10), Bool::False, "is_prime(10)";
is is_prime(11), Bool::True, "is_prime(11)";

# is reduced_digit_sum(777), 3, "reduced_digit_sum(11)";

is collatz1(10), 5, "collatz1";
is collatz(10), <10 5 16 8 4 2 1>, "collatz";

sub fib_test($a) { return 1 if $a <= 1; return fib_test($a-1) + fib_test($a-2) };
is timeit({fib_test(10)}), 0, "timeit"; 

