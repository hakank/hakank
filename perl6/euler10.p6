use v6;

say qq/
Problem 10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
/;

# Answer: 142913828922

# Times:
# 
#  euler10a (array)
#  $n = 200-1  : 0.82s
#  $n = 2000-1 : 4.3s (with grep: 5.5s)
#  $n = 20000-1: 41.9s (with grep: 53.8s)
#  $n = 200000-1: ?
# 
#  euler10b (sets, take 1)
#  $n = 20-1   : 1.3s
#  $n = 200-1  : 1:06 minutes
#  $n = 2000-1 : ? 
#  $n = 20000-1:
#  $n = 200000-1:

#  euler10b2 (sets, take 2)
#  $n = 20-1   : 1.3s
#  $n = 200-1  : 6.7s
#  $n = 2000-1 : > 10 minutes
#  $n = 20000-1:
#  $n = 200000-1:


# my $n = 200-1;
my $n = 2000000-1;


euler10a($n); # array version
# euler10b($n); # set version, I
# euler10b2($n); # set version, II


#
# Using arrays
#
sub euler10a($n) {

    my @primes = 1 xx (1+$n);
    for 2..$n -> $i {
       next if !@primes[$i];

       my $j = 2;
       while $i*$j <= $n {
          @primes[$i*$j] = 0;
          $j++;
       }
    }

    # gather seems to be faster than grep..
    say [+] gather for 2..$n-1 { take $_ if @primes[$_] };
    # say [+] (2..$n-1).grep({ @primes[$_] });
}


#
# Using sets
# This is too slow now...
#
# sub euler10b($n) {

#   my $primes = set 2..$n;

#   for 2..$n -> $i {
#        next if !$primes.exists($i);
#        my $j = 2;
#        while $i*$j <= $n {
#           $primes = $primes.difference(set $i*$j);
#           $j++;
#        }
#     }
#     say $primes;
#     say [+] $primes.keys;
# }

# #
# # Using sets, alternative and faster (than euler10b) version
# #
# sub euler10b2($n) {

#   my $primes = set 2..$n;

#   # weed out the 2's first
#   my $j = 2;
#   my $twos = set ($j..($n/$j).Int).map: {$_*$j};
#   # $primes = $primes.difference($twos);
#   $primes (-)= $twos; # shorter variant
#   say $primes;

#   $j = 3;
#   my $max = $primes.max;
#   while $j <= ($max/$j).Int {
#   # while $j <= ($n/$j).Int {
#      say "j: $j";
#      # $j += 2 and next if !$primes.exists($j);
#      my $s2 = set ($j..($n/$j).Int).map: {$_ * $j};
#      # $primes = $primes.difference($s2);
#      $primes (-)= $s2;
#      $max = $primes.max;
#      $j+=2;
#    }
#     say $primes;
#     say [+] $primes.keys;
# }
