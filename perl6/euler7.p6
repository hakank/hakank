use v6;

say qq/Problem 7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
that the 6^(th) prime is 13.

What is the 10001^(st) prime number?
/;

#
# This tends to take forever (> 24 hour).
#

# thanks to masak for some suggestions of this
use MONKEY_TYPING; augment class Any { 
  method is_prime{
    ?((self == 2|3) || ((!(self%%2))&&all((3,5...* >= sqrt(self)).map: {!(self%%$_)}) )) 
  } 
};

# plain sub:
sub is_prime($n) {
    ?(($n == 2|3) || ((!($n%%2))&&all((3,5...* >= sqrt($n)).map: {!($n%%$_)}) )) 
} 


# say (2...*).grep( {"$_".Int.is_prime()})[^6][*-1];
# say (2...*).grep( {"$_".Int.is_prime()})[^10001][*-1];

# Using the plain sub:
say (2...*).grep( {is_prime($_)})[^10001][*-1];
# say (2...*).grep({is_prime($_)})[^6][*-1]; # 6'th prime

