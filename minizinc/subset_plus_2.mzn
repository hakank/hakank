% 
% Subset problem in MiniZinc.
% 
% From God plays dice:
% "Bitwise set trickery"
% http://gottwurfelt.wordpress.com/2012/07/13/bitwise-set-trickery/
% """
% James Tanton asked last week on Twitter: How many subsets S of {1, 2,... 10} 
% have the property that S and S+2 cover all the numbers from 1 to 12?
% 
% (I’ve taken the liberty of expanding some abbreviations here because my 
% blog, unlike Twitter, doesn’t have a 140-character limit.  Also, Tanton 
% gave a hint which I’m omitting; you can click through to the link if you 
% want it.)
% 
% In case you don’t know the notation, S + 2 means the set that we get from 
% S by adding 2 to each element. So, for example, if S = {3, 5, 9} then 
% S + 2 = {5, 7, 11}.
% """

% And this model (with low=1, high=10, and add=2) gives the expected 25 
% solutions.

% Testing with different values of high (low = 1 and add = 2).
%   high #solutions
%   1    0
%   2    1
%   3    1
%   4    1
%   5    2
%   6    4
%   7    6
%   8    9
%   9   15
%  10   25
%  11   40
%  12   64
%  13  104
%  14  169
%  15  273
%  16  441
%  17  714
%  18 1156
%  19 1870
%  20 3025
% 
% OK, time for OEIS:
% 0,1,1,1,2,4,6,9,15,25,40,64,104,169,273,441,714,1156,1870,3025
% 
% This is http://oeis.org/A074677
% "a(n)=Sum((-1)^(i+Floor(n/2))F(2i+e),(i=0,..,Floor(n/2))), where F(n) = Fibonacci numbers and e=(1/2)(1-(-1)^n)."
% 
% With the first 0 removed it's
% http://oeis.org/A006498
% A006498: a(n) = a(n-1)+a(n-3)+a(n-4).
% Number of compositions of n into 1's, 3's and 4's.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: low = 1;
int: high = 10;
int: add = 2;

% decision variables
var set of low..high: s1;
var set of low+add..high+add: s2;

solve satisfy;

constraint
  forall(i in low..high) (
     i in s1 <-> i+2 in s2
  )
  /\
  s1 union s2 = low..high+add
;

output [
  "s1: " ++ show(s1) ++ "\n" ++
  "s2: " ++ show(s1) ++ "\n"
];

