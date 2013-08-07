use v6;

say qq{
Problem 28

Starting with the number 1 and moving to the right in a clockwise direction a 
5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral 
formed in the same way?
};

say euler28();
say euler28b();


sub euler28 {
  my $s = 0;
  for 3,5...1001 -> $n {
     $s += 4*($n-2)**2 + 10*($n-1);
  }

  return $s+1;

}


sub euler28b {

  return 1+[+] (4*($_-2)**2 + 10*($_-1) for 3,5...1001)
}

