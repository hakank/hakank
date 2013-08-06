% 
%   Fill-in the squares problem (Brainjammer) in MiniZinc.
% 
%   This problem is from the ZDC system, available from 
%   http://www.bracil.net/CSP/cacp/cacpdemo.html , in the
%   file 
%      Brainjammer.txt 
%   from 2003-01-26, which states:
%   """
%   Only Solution is:
%         1	2	3	4	5
%   ====================================================
%   A  	 7	11	2	17	1
%   B	13	19	23	22	3
%   C	9	20	24	14	12
%   D	16	21	25	18	10
%   E	4	8	15	6	5
%
%   22mins55secs of CPU time to find first solution
%   50mins42secs of CPU time with duplicate induced variables removed?
%   Maybe this has something to do with the variable ordering...as this might change
%   as a result of removing duplicate induced variables.
%   1hr:34 mins of CPU time to find a single solution and determine no other solutions
%   exist.
%
%   Statistics for finding the first solution:
%   (with duplicate induced nodes removed)
%   CPU seconds: 		4880.63	(On a Pentium Pro 200Mhz, VC++)
%   Node count:			4036162
%   Induced node count:	1849214
%   Backtracks:			5885311
%
%   """
%
%   Notes: 
%   - On my dual core 3.2 Mhz (Linux) it takes about 0.5 second to solve 
%     this problem, but the comparison is really not fair considering the 
%     difference in machines.
%
%   - The only references to this problem I've found are the following pages:
%     http://discuss.fogcreek.com/techInterview/default.asp?cmd=show&ixPost=2787
%     http://notdarkandstormy.blogspot.com/2005/05/funky-logic-problem.html
%     and especially 
%     http://perplexus.info/show.php?pid=2683
%     which has a lot of comments about manually solving the problem.
%
%   I've yet to know the original source.
%
% Compare with the following model:
% * Comet: http://www.hakank.org/comet/fill_in_the_squares.co

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
set of int: Cell = 1..n*n;

set of int: Primes = {1,2,3,5,7,11,13,17,19,23};
set of int: Primes2 = {1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47};
set of int: Squares = {1,4,9,16,25};

array[0..n-1] of var Cell: a;
array[0..n-1] of var Cell: b;
array[0..n-1] of var Cell: c;
array[0..n-1] of var Cell: d;
array[0..n-1] of var Cell: e;

array[0..n*n-1] of var Cell: ALL; % all numbers

int: max_sum = sum(i in 21..25) (i);
var 1..max_sum: a_sum;
var 1..max_sum: b_sum;
var 1..max_sum: c_sum;
var 1..max_sum: d_sum;
var 1..max_sum: e_sum;


solve satisfy;

constraint
  a_sum == (sum(i in 0..n-1) (a[i])) /\
  b_sum == (sum(i in 0..n-1) (b[i])) /\
  c_sum == (sum(i in 0..n-1) (c[i])) /\
  d_sum == (sum(i in 0..n-1) (d[i])) /\
  e_sum == (sum(i in 0..n-1) (e[i])) /\

  %  Each number from 1-25, used only once
  forall(i in 0..n-1) (
    ALL[i]     == a[i] /\
    ALL[i+n]   == b[i] /\
    ALL[i+2*n] == c[i] /\
    ALL[i+3*n] == d[i] /\
    ALL[i+4*n] == e[i]
  )
  /\
  all_different(ALL) /\


  % 1.Sum of each column is odd
  forall (i in 0..n-1) (
    (a[i] + b[i] + c[i] + d[i] + e[i]) mod 2 == 1
  )
  
  /\
  % 2.Sum of each row, except C is even
  a_sum mod 2 == 0 /\
  b_sum mod 2 == 0 /\
  c_sum mod 2 == 1 /\
  d_sum mod 2 == 0 /\
  e_sum mod 2 == 0 /\

  
  % 3.Sum of row A is not greater than the sum of any other row
  a_sum <= b_sum /\	
  a_sum <= c_sum /\	
  a_sum <= d_sum /\	
  a_sum <= e_sum /\	

  
  % 4.The sum of diagonal A1 to E5 is greater than the sum of
  %   diagonal E1 to A5
  a[0] + b[1] + c[2] + d[3] + e[4]  > e[0] + d[1] + c[2] + b[3] + a[4] /\
  
  % 5.(A4 + B4) is greater than (C4+D4+E4)
  a[3] + b[3] > c[3] + d[3] + e[3] /\
  
  % 6. A1 + B1 = D1 + E1
  a[0] + b[0] == d[0] + e[0] /\ 
         
  % 7. A1 > E1
  a[0] > e[0] /\ 
  
  % 8. A1, A3 and B1 are primes
  a[0] in Primes /\
  a[2] in Primes /\
  b[0] in Primes /\
  
  % 9.(A3 + E3) is a prime number
  (a[2]+e[2]) in Primes /\

  
  % 10. A5,D1,D3 and E1 are squares
   a[4] in Squares /\
   d[0] in Squares /\ 
   d[2] in Squares /\ 
   e[0] in Squares /\ 

  % 11. B2, C2, and D2 are ascending consecutive numbers
  b[1] + 1 == c[1] /\ 
  c[1] + 1 == d[1] /\ 
  
  % 12. B3, C3, and D3 are ascending consecutive numbers
  b[2] + 1 == c[2] /\ 
  c[2] + 1 == d[2] /\ 
  
  % 13. B5 + D5 = A5 + C5
  b[4] + d[4] == a[4] + c[4] /\ 
  
  % 14. (c1)^2 + (c5)^2 = (e3)^2
  c[0]*c[0] + c[4]*c[4] == e[2]*e[2] /\
  
  % 15. C5 is a two-digit number
  c[4] > 9 /\ 
  
  % 16. D5 is a multiple of E5
  d[4] mod e[4] == 0 /\ 
  
  % 17. E1 + E3 = E2 + E4 + E5
  e[0] + e[2] == e[1] + e[3] + e[4]
;

% output [show(ALL)];
output
[
 "a: " ++ show(a) ++ "\n" ++
 "b: " ++ show(b) ++ "\n" ++
 "c: " ++ show(c) ++ "\n" ++
 "d: " ++ show(d) ++ "\n" ++
 "e: " ++ show(e) ++ "\n"

];