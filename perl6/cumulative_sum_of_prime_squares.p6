use v6;

# Cumulative sum of prime squares.

# Thanks to masak for ideas on this.

my @p = ([\+] ((2..150).grep({$_.is-prime})).map: {$^a**2});
say @p;
say @p[*-1];
