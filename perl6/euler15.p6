use v6;

say qq/
Problem 15

Starting in the top left corner of a 2×2 grid, there are 6 routes 
(without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?
/;

say ([*] 21..40) / ([*] 2..20);