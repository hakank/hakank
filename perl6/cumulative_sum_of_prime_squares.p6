use v6;

# Cumulative sum of prime squares.

# Thanks to masak for ideas on this.
use MONKEY_TYPING; 
augment class Any { method is_prime{?((self == 2|3) || ((!(self%%2))&&all((3,5...* >= sqrt(self)).map: {!(self%%$_)}) )) } }

say ([\+] ((2..50).grep({$_.Str.Int.is_prime})).map: {$^a**2}).perl;
say ([\+] ((2..17).grep({$_.Str.Int.is_prime})).map: {$^a**2}).perl;
say ([\+] ((2..17).grep({$_.Str.Int.is_prime})).map: {$^a**2})[*-1];
