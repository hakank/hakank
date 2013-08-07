use v6;

# Euler 9
#
say qq/
Problem 9

A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
/;

sub is_pyth($a, $b, $c) { return $a**2 + $b**2 == $c**2 };

my $n = 1000;

#
# Solving time original version: 3:05 minutes
# Solving time version 2       : 24.875s
# Solving time version 3       : 3.8s
#
my $found_sol = 0; # version 3
for (1..($n/2).Int) -> $c {
  last if $found_sol; # version 3
  next if ($n-$c/2).Int - $c > $c; # version 2
  for ($c,($c-1)...(($n-$c/2).Int - $c)) -> $b {
       my $a = $n - $b - $c;
       if $a > 0 and is_pyth($a, $b, $c) {
          say "$a*$b*$c == {$a*$b*$c}";
          $found_sol++;
          last;
       }
  }
}


