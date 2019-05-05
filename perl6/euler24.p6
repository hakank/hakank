use v6;

say qq{
Problem 24

A permutation is an ordered arrangement of objects. For example, 3124 is one 
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
listed numerically or alphabetically, we call it lexicographic order. The 
lexicographic permutations of 0, 1 and 2 are:

   012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 
0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
};

# 0.16s
say ithperm(10, 999999);

# This is much slower: 7.9s
# my @perm = (0..9).permutations;
# say @perm[999_999];

# Though this is faster; 1.5s
# say (0..9).permutations[999_999];


#
# Masak wrote the Perl 5 version of this after a discussion how
# to extract the i'th permutation.
#
# I (hakank) then ported it to Perl 6.
#
sub ithperm($length is copy, $index is copy) {
    my $total_number = [*] 1..$length;
    my @digit_set = 0..$length-1;
    my $result = "";

    for 1..$length {
       $total_number /= $length;
       my $next_digit = @digit_set[($index / $total_number).Int];
       @digit_set = @digit_set.grep({ $_ != $next_digit });
       $index %= $total_number;
       --$length;
       $result ~= $next_digit;
    }
    return $result;
}

