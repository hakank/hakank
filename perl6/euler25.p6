use v6;

say qq{
Problem 25

The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
};


# Note: Rakudo don't yet support arbitrary precision so let's fake it.
my $limit = 1000 | 15;

my $n = 0;
for (1,1,*+*...*) -> $f {
    $n++;
    my $len = $f.Str.bytes;
    say "$n: $f ($len)";
    last if $len >= $limit;
}
