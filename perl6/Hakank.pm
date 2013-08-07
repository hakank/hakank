use v6;

# 
# Note: If the APL chars (such as ⊥) is messed up,
# please see the HTML version of this file:
# http://www.hakank.org/perl6/Hakank.pm.html
#

#
# Some utilities.
#
# Mostly inspired by Haskell, FP in general, 
# APL, K, and some R.
#
#
# Hakan Kjellerstrand (hakank@bonetmail.com)
# My Perl 6 page: http://www.hakank.org/perl6/
# 
#

module Hakank;

#
# diff(@a)
# Takes the difference between consecutive numbers
# in array @a.
#
# Example:
#   > my @fib = (1,1,*+*...*)[^10]
#   > diff @fib
#   0 1 1 2 3 5 8 13 21
#   > @fib.&diff
#   0 1 1 2 3 5 8 13 21
#   > @fib.&diff.&diff
#   1 0 1 1 2 3 5 8
#
# Note: For higher order of diff, see diffn()
#
sub diff(@a) is export { @a[1..*-1] <<->> @a[0..*-2] };


#
# diffn(@a, $n)
#
# Higher order difference between consecutive numbers.
# 
# Example:
#   > my @fib = (1,1,*+*...*)[^10]
#   > diffn(@fib, 2)
#   1 0 1 1 2 3 5 8
# 
sub diffn(@a, $n) is export { my @t=@a; @t = diff(@t) for 1..$n; return @t};


#
# Peaks and valleys: 
# 
# Returns the positions in the sequence @a where there are 
# peaks/valleys. Here we analyze the signs of the &diff on the
# sequence.
#
# Example:
#  Positions 0  1  2  3  4  5  6  7
#  Sequence <1  2  3  4  7  6  8  5>
#            1  1  1  1 -1  1 -1    (sign of differences
#                        ^  ^  ^
#                        |  |  |
# 
# Here the peaks/valleys are between 
#     7->6 
#     6->8
#     8->5
# i.e. positions 4, 5, 6
# > peaks_valleys(<1 2 3 4 7 6 8 5>)
# [4, 5, 6]
#
#
# The adjustment is needed when there is "plateaus" in the
# sequence, i.e. when the sign diff sequence contains 0.
# Example: 
# The sequence <1 2 3 4 7 7 5 5 8 8 9 9 5 6 6 7>
# [  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  (positions) ]
# a: 1  2  3  4  7  7  5  5  8  8  9  9  5  6  6  7  (sequence
# s: 1  1  1  1  0 -1  0  1  0  1  0 -1  1  0  1     (sign diff original)
# s: 1  1  1  1  1 -1 -1  1  1  1  1 -1  1  1  1     (sign diff after adjustment)
#                   ^     ^           ^  ^           changes
#      	       	    |     |           |  |
#                  (5)   (7)        (11)(12)
# 
#
# > peaks_valleys(<1 2 3 4 7 7 5 5 8 8 9 9 5 6 6 7>
# [5, 7, 11, 12]
#
sub peaks_valleys(@a) is export {

    # the signs of the diffs
    my @s = @a.&diff>>.sign; 
    say "a: @a[]\ns: {@s}";

    # # adjust for 0 in diff, i.e. same value neighbours
    for 1..(+@s)-1 {  @s[$_] = @s[$_-1] if @s[$_] == 0 and @s[$_-1] != 0 }
    say "s: {@s} (after adjustment)";

    # get the positions of peaks/valleys
    my @pos = gather for 1..(+@s)-1 { take $_ if @s[$_] != @s[$_-1] };

    say "POS: @pos[]";
    .say for @pos.map: {"$_: {@a[$_]}"}

    return @pos;
};


# 
# repeat(&code, $times=1)
# 
# Repeats &code $times times
# 
# Example:
#   > repeat(  { ((1..10).roll(10).&mean) }, 5)
#   4.6 5.3 4.9 4.9 6.9
#   
#   # Birhtday "paradox" simulation
#   > .say for gather for 20..30 -> $n { my $r=100; take $n => repeat( {? (any((table((1..365).roll($n))).values) > 1) }, $r).&sum() / $r }
#      20  0.43
#      21  0.41
#      22  0.47
#      23  0.47
#      24  0.61
#      25  0.51
#      26  0.63
#      27  0.66
#      28  0.56
#      29  0.69
#      30  0.71 
#
sub repeat(&code, $times=1) is export { (code() for 1..$times) };


#
# mean(@a)
# 
# Mean value of values in array @a
#
# Example:
#   > mean((1..10).roll(10))
#   4.8
#   > (1..10).roll(10).&mean
#   4.8
#   > (1..10).roll(10) ==> mean
#   4.2
#
sub mean(@a) is export { ([+] @a) / @a.elems };


#
# sum(@a) 
# 
# Sums the value of @a. 
# (I wanted something that I could chain easily.)
# 
# Example:  
#   > sum(1..10)
#   55
#   > (1..10).&sum
#   55
#
sub sum(@a) is export { [+] @a };



#
# table(@a)
#
# Collects the value of @a as a hash with occurrences.
# Example:
#   > my @a = <8 1 7 10 9 8 3 7 10 10>
#   > table(@a)
#   8   2
#   1   1
#   7   2
#   10  3
#   9   1
#   3   1
#
#   # order by decreasing value
#   >  .say for @a.&table().sort({-.value})
#   10 3
#   8  2
#   7  2
#   1  1
#   9  1
#   3  1
#
# Inspiration: R's table()
#
sub table(@a) is export { my %h = (); for @a {%h{$_}++}; return %h };


#
# positions(@a, &code)
#
# Returns the positions for the elements in @a where &code is 
# evaluated to Bool::True.
#
# Example:
#   > positions(1..20, {$_ %% 3})
#   2 5 8 11 14 17
#   # And here we got back the values
#   > (1..20)[positions(1..20, {$_ %% 3})]
#   3 6 9 12 15 18
#
sub positions(@a, &code) is export {
    # gather for @a.map({code($_)}) Z 0..@a.elems-1 { take $^b if $^a } 
    # More clear:
    gather for @a.pairs { take .key if code(.value)}
};


#
# bool2pos(@a)
#
# Returns the positions in @a for which the values are true.
#
# Example:
# > bool2pos(<0 1 0 1 0 1 0 1 0>
# (1, 3, 5, 7)
#
# > bool2pos([2==1,"hakan", 1, 0, 10 > 2])
# (1, 2, 4)
sub bool2pos(@a) is export {

    gather for @a.pairs { take .key if .value}
};

# 
# pos2bool(@a)
#
# Returns a boolean list (of length @a.elems)
# where are the true values in @ are represented as 1,
# and the false with 0
# 
# Example:
# > pos2bool((0..10).map: {$_%%3})
# (1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
#
sub pos2bool(@a) is export {

    @a.map: { $_ ?? 1 !! 0}
}


# 
# compress(@a, @b)
#
# Returns the elements in @a for which the elements in 
# the boolean list @b is true.
# 
# Example:
# > my @a = <0 1 2 3 4 5>
# > my @b = <1 0 1 1 0 1>
# > compress(@a, @b)
# (0, 2, 3, 5)
#
# > my @a = "ABCDEF".split("")
# > my @b = <1 0 1 1 0 1>
# > compress(@a, @b)
# ACDF
# 
# Inspiration: APL's compress ("/"):
#       1 0 1 1 0 1 / 'ABCDEF'
#    ACDF 
#
sub compress(@a, @b) is export {
    return @a[bool2pos(@b)]
};


#
# Infix version of compress: ///
#
# Make compress(@a, @b) more like APL's "/"
#
# Example:
# > my @a = "ABCDEF".split("")
# > my @b = <1 0 1 1 0 1>
# > @b /// @a
# ABDF
#
our sub infix:<///>(@b, @a) is export {
    return compress(@a, @b)
};

#
# group(@a)
#
# Groups the values of an array into separate partitions.
# Returns the "equivalency classes" of the positions
# for the same values.
#
# Cf classify for hashes.
#  
# Example:
#   > my @a = <1 1 1 2 2 13 1 13 1>
#   > @a.&group
#   1	0 1 2 6 8
#   2	3 4
#   13	5 7
#
# Inspiration: K:s group function (=):
#    =1 1 1 2 2 13 1 13 1 
#    (0 1 2 6 8
#    3 4
#    5 7)
#
sub group(@a) is export { my %h=(); for 0..+@a-1 { %h.push( @a[$_] => $_)}; %h; };


#
# head(@a) 
# 
# Head (first) element of an array.
#
# Example:
#   > my @a = 1..10
#   > head(@a)
#   1
#   > @a.&head
#   1
#   > @a ==> head
#   1
# 
sub head([$head, *@tail]) is export { $head };


# 
# tail(@a)
# 
# Returns the tail (all but the first element) of an array.
#
# Example:
#   > my @a = 1..10
#   > tail(@a)
#   2 3 4 5 6 7 8 9 10
#   > head(tail(@a))
#   2
#   > @a ==> tail ==> head
#   2
#
sub tail([$head, *@tail]) is export { @tail };

# 
# butlast(@a)
# 
# Returns all but the last element in an array.
# 
# Example:
#   > my @a = 1..10
#   > butlast(@a)
#   1 2 3 4 5 6 7 8 9
#
sub butlast(@a) is export { @a[0...@a.elems-2] };

# 
# take_while(@a, &cond)
#
# Takes the elements in order until &cond evaluates to Bool::True.
# 
# Example:
#   > my @a = 1..10
#   > take_while(@a, ( * < 4 ) )
#   1 2 3
#   > take_while(@a, { $_ < 4 } )
#   1 2 3
#   > @a.&take_while( * < 4 )
#   1 2 3
#
#   # Coupon collector's problem:
#   # How many times must a die be thrown to get all 6 values?
#   # (simulate 100 runs)
#   > .say for repeat({my @c=();((1..6).roll(*)).&take_while({@c.uniq.elems < 6 and @c.push($_) }).elems; @c.elems}, 100).&table.sort({-.value})
#
sub take_while(@a, &cond) is export { ($_ if cond($_)) or last for @a  };

#
# drop_if(@a, &cond)
# 
# Skip all elements that evaluates to Bool::False.
# 
# Example:
#   > my @a = 1..10
#   > @a.&drop_if(* < 5)
#   5 6 7 8 9 10
#
sub drop_if(@a, &cond) is export { $_ if !cond($_) for @a };

# 
# if_do_else(@a, &cond, &do, &else)
#
# An array version of if then else (kinda).
# For each elements in @a test
#    if &cond {
#       do()
#    } else {
#       else()
#    }
#
# Example:
#   > if_do_else(@a, {$_ %% 3}, {say $_}, {say "no"})
#   no
#   no
#   3
#   no
#   no
#   6
#   no
#   no
#   9
#   no
#   > if_do_else(@a, {$_ %% 2}, {$_ / 2 }, { 1  + ($_ * 3) })
#   4 1 10 2 16 3 22 4 28 5
#
sub if_do_else(@a, &cond, &do, &else) is export { for @a { cond($_) ?? do($_) !! else($_) }  };


# 
# $u gcd $v
#
# Infix version of gcd($u, $v)
#
# Example:
#   > 10 gcd 4
#   2
#
our sub infix:<gcd>($u, $v) is export { if $v == 0 { abs($u) } else { $v gcd ($u % $v) } } ;

# 
# $a lcm $b
#
# Infix version of lcm($a, $b)
#
# Example:
#   > 10 lcm 4
#   20
#   # From Euler problem 5
#   > [lcm] 2..20
#   232792560
#
our sub infix:<lcm>($a, $b) is export { return $a * $b / ($a gcd $b) };

#
# is_prime($n)
#
# Check is $n is a prime. Not very effective or fast 
# for larger $n.
#
# Assumes that $n is positive.
#
# Example:
#   > is_prime(10)
#   Bool::False
#   > is_prime(11)
#   Bool::True
#   > 10.&is_prime
#   Bool::False    
#   > 11.&is_prime
#   Bool::True
#   > ( 1* is_prime($_) for 1..10)
#   0 1 1 0 1 0 1 0 0 0
sub is_prime($n) is export {
    ?( 
       ($n > 1) && 
       ( 
          ($n== 2|3) 
          || 
          ( (!($n%%2)) && 
            all((3,5...* >= sqrt($n)).map: { !( $n %% $_ ) }) 
          )
       )
     ) 
};

#
# next_prime($n)
#
# Returns the next prime from $n
#
# Example:
#   > next_prime(10)
#   11
#   > next_prime(11)
#   13
#   > 2,{next_prime($_)}...*>30
#   2 5 7 11 13 17 19 23 29 31
#
sub next_prime($n) is export {
        my $s=$n+($n%%2 ?? 1 !! 2); 
        for $s,$s+2...* { return $_ if is_prime($_) } 
}


#
# Primes the APL way: Calculate the primes below $n
# 
# This was inspired by APL's
#   (~R∊R∘.×R)/R←1↓⍳R
#
# > my @i=2..10; @i.grep({!(@i X<<*>> @i).grep($_)})
# 2 3 5 7
# 
# slightly faster
# > my @i = 2..10; my @j=(@i X<<*>> @i).uniq; @i.grep({!@j.grep($_)})
# 2 3 5 7
#
sub primes_slow($n) is export {

    # my @i=2..$n; return @i.grep({!(@i X<<*>> @i).grep($_)})
    # slightly faster
    # my @i=2..$n;  my @j=(@i X<<*>> @i).uniq; return @i.grep({!@j.grep($_)});
    
    # some more tweaking
    my @i=2..(1+($n/2).Int); my @j = (@i X<<*>> @i); (2..$n).grep({!(@j.grep($_))})
};


#
# reduced-digit-sum($x)
#
# Calculates the reduced digit sum of a number, or an array 
# (which sums first).
#
# Example:
#   > reduced_digit_sum(777)
#   3
#   > my %alpha = "a".."z" Z=> (1..26)
#   > %alpha{"bach".split("")}
#   2 1 3 8
#   > reduced_digit_sum(%alpha{"bach".split("")})
#   5
#
# TO CHECK: Got the following error on this:
#    Unable to parse blockoid, couldn't find final '}' at line 282
# (Which is strange, since it works in the Rakudo REPL...)
#
# sub reduced_digit_sum($x) is export { (([+] $x.comb),{[+] .comb}...*<10)[*-1] };


#
# collatz($n)
# collatz1($n)
#
# collatz($n) calculates the Collatz (Hailstone) sequence.
# collatz1($n) just returns the next value in the sequence.
#
# Example:
#  > collatz(10)
#  10 5 16 8 4 2 1
#
#  > collatz1(10)
#  5
#
sub collatz1($n) is export { return ($n %% 2) ?? ($n / 2) !! (1 + $n * 3) };
sub collatz($n) is export { return ($n,{collatz1($^x)}...1) };


#
# timeit(&code)
#
# Get the time to run code().
#
# Example:
#   > sub fib($a) { return 1 if $a <= 1; return fib($a-1) + fib($a-2) } 
#   > say timeit({ fib(23) })
#   7
#   > {fib(23)}.&timeit
#   7
#   > {(1..20)>>.&fib}.&timeit
#   4
#
sub timeit(&code) is export { my $t0 = time; code(); my $t1 = time; $t1 - $t0 };


sub even($n) is export { $n %% 2 };
sub odd($n) is export { !even($n) };


# 
# Transitive closure ("pointer chasing")
#
# tc(list, start)
# 
# Walks through the list using indices with start value (index) start
#  
# Examples:
#
#   > tc(<1 2 3 4 0>, 1)
#   1 2 3 4 0  
#   > tc(<2 1 0 4 5 3>, 4)
#   4 5 3
#   
#   > tc(<0 2 3 0 1 4>, 5)
#   5 4 1 2 3 0
#
#   # The length of the resulting list can never be larger than
#   # the source list, so we stop (instead of infinitive loop)
#   > tc(<1 2 2 3 4 0>, 1)
#   1 2 2 2 2 2
#
#   > for 0..5 { say "$_: ",  tc(<1 2 3 5 0 4>, $_).join(" ") }
#   0: 0 1 2 3 5 4
#   1: 1 2 3 5 4 0
#   2: 2 3 5 4 0 1
#   3: 3 5 4 0 1 2
#   4: 4 0 1 2 3 5
#   5: 5 4 0 1 2 3
#
# This was inspired by K:s transitive closure function 
# "over until fixed" (\) i.e. list\start:
#       (2 1 0 4 5 3)\4
#   4 5 3
# 
sub tc(@a, $start) is export { 
    my $a_len = +@a; 
    my @c=(my $c=$start); 
    while @c.push($c = @a[$c]) and @c[*-1] != @c[0] and +@c < $a_len {}; 
    pop @c; 

    return @c;
};

# alternative version
sub tc2(@a, $start) is export {
    my @c = ($start,{@a[$^x]}...{ +@_>= +@a && @_[*-1] !=@_[0]}); 
    pop @c; 
    return @c;
};

# our sub infix:<\\>(@a, $init) is export {
#     return tc(@a, $init)
# };


#
# sort_perm: Return the sort permutation of an array.
# 
# Right now I assume that we are using Ints.
# 
# > my @x = <13 2 0 1 4>; say sort_perm(@x).perl
# (2, 3, 1, 4, 0)
# And using this permutation we get a sorted array
# > @x[sort_perm(@x)]
# 0 1 2 4 13
#
# Without .Int we can sort strings
# my @hk = "hakank kjellerstrand".split("")
# @hk[@hk.pairs.sort({.value}).map: {.key}]
# 6 1 3 17 19 9 12 0 8 2 5 7 10 11 4 18 13 16 14 15
#
# Inspiration: K's "<" (sort) and APL's ⍋ (Grade Up)
#
sub sort_perm(@a) is export {

    # why don't .keys work, i.e.
    #    @a.pairs.sort({.value}).keys 
    return @a.pairs.sort({.value.Int}).map: {.key}
};

#
# And now we make this as APL Grade Up
#
# > ⍋ <13 2 0 1 4>
# 2 3 1 4 0
# # Double Grade Up gives the ranking of each element 
# > ⍋⍋ <70 10 30 20>
# 3 0 2 1
our sub prefix:<⍋> (@a) is export { sort_perm(@a) };
# our sub prefix:<⍋>(@a) is export { @a.pairs.sort({.value.Int}).map: {.key} };



# 
# APL each (¨)
#
# Just another (syntactic) way of doing a map (gather for loop).
# 
# Example:
# > each({2*$^x}, 1..10)
# > {2*$^x}¨(1..10)
# (2, 4, 6, 8, 10, 12, 14, 16, 18, 20) 
#
# Like a grep
# > {2*$^x+1 if $^x %% 2}¨(1..10)
# (5, 9, 13, 17, 21) 
#
# sub each(&f, @a) is export { gather for @a { take &f($_) } };
sub each(&f, @a) is export { &f($_) for @a };
sub infix:<¨>(&f,@a) is export { each(&f, @a) };


#
# APL's Base:
#
# base(@radix, @a)
# Convert @a to radix @radix.
#
# >  base([10, 10, 10, 10], <4 1 2 3>)
# 4123
# 3 >  base([0, 3, 12], <3 0 1>)
# 109
#
# APL: 
#    1736 3 12 ⊥ 3 0 1
#  109
#    0 3 12 ⊥ 3 0 1
#  109
#    10 10 10 10 ⊥ 4 1 2 3
#  4123
#
sub base(@radix, @a) is export {[+]([\*](1,@radix.splice(1,+@radix))).reverse <<*>> @a };

#
# More APL like version of base (⊥)
# 
# Example:
# > [0,3,12]⊥<3 0 1>
# 109
# [10,10,10,10]⊥<4 1 3 2>
# 4132
# [2,2,2,2]⊥<1 0 1 1>
# 11
#
sub infix:<⊥>(@radix, @a) is export { base(@radix, @a) };



#
# Unbase
# 
# Given a radix and number, convert the number into the
# radix slots.
# 
# > unbase(<10 10 10 10>, 1234
# (1, 2, 3, 4)
#
# > unbase(<24 60 60>, 12345)
# 3 25 45
#
# We add an extra element if the $value is too large
#
# > unbase(<24 60 60>, 123456)
# 1 10 17 36
#
# > unbase(<24 60 60>, 3)
# 3
#
#
# Inspiration:
#   APL's unbase (⊤)
#      10 10 10 10 ⊤ 1234
#    1 2 3 4
#
#    24 60 60 ⊤ 123456
#    10 17 36
# We assume a leading "0" in this implementation, as
# in this construct. E.g. as in this APL code:
#     0 24 60 60 ⊤ 123456
#  1 10 17 36
#
sub unbase(@radix, $val) is export { 
    my $v = $val;
    my @res = gather for @radix.reverse { take $v % $_; $v = ($v / $_).Int };
    @res.push($v) if $v >= 1;
    return @res.reverse
}


#
# And as an APL like operator.
# Note: This is not the letter "T", but
# the APL char ⊤ (on the N key on an APL keyboard).
# 
# > <10 10 10 10> ⊤ 1234
# 1 2 3 4
# > <24 60 60> ⊤ 123456
# 1 10 17 36
#
sub infix:<⊤>(@radix, $val) is export { unbase(@radix, $val) };
