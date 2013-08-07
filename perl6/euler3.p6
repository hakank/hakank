use v6;

# Problem 3

say qq/
Euler 3: 
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
/;

# Tend to generate core dump.
# my$n=600851475143; say [max] (2..sqrt($n+1)).grep: {my$x=$_;($n%%$x)&&all((2..sqrt($_)).map: {!($x%%$_)})};

# Smaller example...
# my$n=13195; say [max] (2..sqrt($n+1)).grep: {my$x=$_;($n%%$x)&&all((2..sqrt($_)).map: {!($x%%$_)})};

# This took about 8:11.54min in Rakudo
#                    2.2seconds in Niescza
# my $n = 13195;
my $n = 600851475143;
my $i = sqrt($n).Int;

# ensure oddity if $i
$i++ if !$i%%2;

say "$i start";

my $found = 0;
my $c = 0;
while $i > 0 and !$found {
    if $n %% $i and is_prime($i) {
        $found = 1;
    } else {
        $i = $i-2;
    }
    $c++;
    #if $c % 1000 == 0 {
    #    print ".";
    #}
}

say "\nFound: $i";


sub is_prime($n) {
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
