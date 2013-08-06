% 
% Enigma problem 1570 (Set of cubes) in MiniZinc.
% 
% From New Scientist:
% http://www.newscientist.com/article/mg20427330.800-enigma-number-1570.html
% """
% 04 November 2009 by Richard England
% 
% Sets of cubes
% 
% 1. Find the set of perfect cubes that between them use each of the 
% digits 0 to 9 at least once whose sum is as small as possible. What 
% is the sum of your set of cubes?
% 
% 2. Find the set of perfect cubes that between them use each of the digits 
% 0 to 9 exactly once whose sum is as small as possible. What is the sum 
% of your set of cubes?
% 
% In answering either of these questions you may, if you wish, 
% treat 0 as itself being a cube.
% """

% Note: This model handles just the second problem, i.e. where all 
%       digits are used exactly once.
%
% Answer:
%  num1 = 1
%  num2 = 8
%  num3 = 64
%  num4 = 205379
%  total = 205452
%  x = [1, 8, 6, 4, 2, 0, 5, 3, 7, 9]
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
int: len = 10; % length of the array
set of int: cubes;

array[1..len] of var 0..9: x;
var int: num1; % first number
var int: num2; % second number
var int: num3; % third number
var int: num4; % fourth number
var 1..len: len1;           % length of first number
var 1..len: len2;           % length of second number
var 1..len: len3;           % length of third number
var int: total;

%
% Convert array <-> number in some base
%
predicate toNum(array[int] of var 0..9: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
;

% It is much faster if we don't have x in the labeling
% solve satisfy;
solve :: int_search(
         [total,num1,num2,num3,num4,len1,len2,len3], % ++ x,
         smallest,
         indomain_min, 
         complete) 
     % satisfy;
     minimize total;

constraint

  all_different(x)

  /\
  total = num1 + num2 + num3 + num4

  /\
  num1 in cubes /\
  num2 in cubes /\
  num3 in cubes /\
  num4 in cubes

  /\ % num1. 
  exists(j in 1..len) (
     j = len1 /\
     toNum([x[i] | i in 1..j], num1)
  )

  /\  % num2
  exists(j, k in 1..len) (
     j = len1+1 /\ 
     k = len1+len2 /\ k >= j  /\
     toNum([x[i] | i in j..k], num2)
  )

  /\ % the num3 
  exists(k,l in 1..len) (
     k = len1+len2+1 /\
     l = len1+len2+len3 /\ l >= k  /\
     toNum([x[i] | i in k..l], num3)
  )

  /\ % the num4
  exists(l in 1..len) (
     l = len1+len2+len3+1 /\
     toNum([x[i] | i in l..len], num4)
  )

  /\ % symmetry breaking
  num1 < num2
  /\
  num2 < num3
  /\
  num3 < num4

;

%
% Here are cubes between 0^3..1000^3 which just contains unique digits.
%
cubes = {
0,
1,
8,
27,
64,
125,
216,
512,
729,
1728,
2197,
4096,
4913,
5832,
6859,
9261,
10648,
13824,
19683,
24389,
32768,
42875,
54872,
68921,
205379,
287496,
328509,
389017,
421875,
438976,
592704,
681472,
804357,
912673,
2460375,
3048625,
8365427,
24137569,
26198073,
27543608,
32461759,
};

output
[
  "x: " ++ show(x)
];
