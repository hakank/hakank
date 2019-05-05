use v6;

say qq/Problem 7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
that the 6^(th) prime is 13.

What is the 10001^(st) prime number?
/;

say (2...*).grep({is-prime($_)})[^10001][*-1];

