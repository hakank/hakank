% 
% Average avoiding in MiniZinc.
% 
% From Stack Overflow
% http://stackoverflow.com/questions/19345090/arrange-an-array-such-that-the-average-of-2-numbers-does-not-lie-between-them
% """
% As the question says, find an algorithm to arrange the array. This was a 
% Facebook interview question.
% 
% The average needs to be exact. We do not round of, take floor or ceil of the average.
% 
% Edit : To quote an example, if the numbers are 1,2,5,9, then the arrangement 
% {1,9,2,5} is valid but {1,5,9,2} is not since average of 1 and 9 is 5 and lies 
% between them. Why is everybody downvoting this?
% 
% """


% For the 0..n-1, Picat (using occurrence/indomain_split and the even symmetries) solves N=100
% in 25:06.25 minutes:
% 
% x: [0, 64, 96, 32, 48, 80, 16, 56, 88, 24, 40, 72, 60, 92, 8, 28, 44, 76, 12, 52, 84, 20, 36, 68, 62, 94, 4, 30, 46, 78, 14, 54, 86, 22, 38, 70, 58, 90, 6, 26, 42, 74, 10, 50, 82, 98, 18, 34, 66, 2, 1, 65, 97, 33, 49, 81, 17, 57, 89, 25, 41, 73, 61, 93, 9, 29, 45, 77, 13, 53, 85, 21, 37, 69, 63, 95, 5, 31, 47, 79, 15, 55, 87, 23, 39, 71, 59, 91, 7, 27, 43, 75, 11, 51, 83, 99, 19, 35, 67, 3]
% x diffs: [64, 32, -64, 16, 32, -64, 40, 32, -64, 16, 32, -12, 32, -84, 20, 16, 32, -64, 40, 32, -64, 16, 32, -6, 32, -90, 26, 16, 32, -64, 40, 32, -64, 16, 32, -12, 32, -84, 20, 16, 32, -64, 40, 32, 16, -80, 16, 32, -64, -1, 64, 32, -64, 16, 32, -64, 40, 32, -64, 16, 32, -12, 32, -84, 20, 16, 32, -64, 40, 32, -64, 16, 32, -6, 32, -90, 26, 16, 32, -64, 40, 32, -64, 16, 32, -12, 32, -84, 20, 16, 32, -64, 40, 32, 16, -80, 16, 32, -64]


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% int: n = 4;
% array[1..n] of int: a = [1,2,5,9];
% int: n = 9;
% array[1..n] of int: a = [1,2,3,4,5,6,7,8,9];
% int: n = 3;
% array[1..n] of int: a = [2,3,4];

% From one of the comments.
% This is UNSAT according to this model.
% int: n = 10;
% array[1..n] of int: a = [2,2,2,4,5,6,2,2,2,2];

% From one of the comments:
int: n = 40;
int: n2 = n div 2;
array[1..n] of int: a = [ i | i in 0..n-1];

set of int: s = { a[i] | i in 1..n  };
% array[1..card(s)] of int: counts = [ sum([bool2int(a[i] == v) | i in 1..n ]) | v in s];

% decision variables
array[1..n] of var s: x;

solve satisfy;
% solve :: int_search(x, occurrence, indomain_split, complete) satisfy;
% solve :: int_search(x, most_constrained, indomain_split, complete) satisfy;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint
  forall(i,j in 1..n where i < j) (
    forall(k in i+1..j-1) (
       % 2*x[k] != (x[i] + x[j])
       x[k]+x[k] != x[i] + x[j]
    )    
  )

  /\ % symmetry breaking
  minimum(x[1], x)

  % /\ % symmetry breaking for even n
  % if n mod 2 == 0 then
  %   forall(i in 1..n div 2) (
  %      x[i+n2] = x[i]+1
  %   )
  %   /\
  %   forall(i in 1..n) (
  %      if i < n div 2 then
  %        x[i] mod 2 = 0
  %      else
  %        true
  %      endif
  %      /\
  %      if i > n div 2 then
  %        x[i] mod 2 = 1
  %      else
  %        true
  %      endif
  %   )
  % else 
  %    true
  % endif
  % /\
  % alldifferent(x) :: domain % experiment!

  % % symmetry breaking (TESTING)
  % /\ x[1] = 0

  % from a comment (0..99)
  % /\ cp1d(x, [0,64,32,96,16,80,48,8,72,40,24,88,56,4,68,36,20,84,52,12,76,44,28,92,60,2,66,34,98,18,82,50,10,74,42,26,90,58,6,70,38,22,86,54,14,78,46,30,94,62,1,65,33,97,17,81,49,9,73,41,25,89,57,5,69,37,21,85,53,13,77,45,29,93,61,3,67,35,99,19,83,51,11,75,43,27,91,59,7,71,39,23,87,55,15,79,47,31,95,63])

  %% Experiments
  % /\ % count occurrences of v in a
  %    % (since we can't assume distinct values)
  % forall(v in s) (
  %    let {
  %       int: c = sum([bool2int(a[j] == v) | j in 1..n])
  %    } in
  %    count(x, v, c)
  % ) 
  /\ % count occurrences of v in a
     % (since we can't assume distinct values)
  forall(v in s) (
     let {
       var 1..n: c
     } in
     count(a, v, c) % :: domain 
     /\
     count(x, v, c) % :: domain
  ) 


;

output [
  "a: " ++ show(a) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++ 
  "x diffs: " ++ show([x[i]-x[i-1] | i in 2..n]) ++ "\n" ++
  "x mod 2: " ++ show([x[i] mod 2 | i in 1..n]) ++ "\n"
];

