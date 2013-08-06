% 
% Crossfigure problem in MiniZinc.
%
% CSPLib problem 21
% http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob021/index.html
% """
% Crossfigures are the numerical equivalent of crosswords. You have a grid and some 
% clues with numerical answers to place on this grid. Clues come in several different 
% forms (for example: Across 1. 25 across times two, 2. five dozen, 5. a square number, 
% 10. prime, 14. 29 across times 21 down ...). 
% """
%
% Also, see 
% http://en.wikipedia.org/wiki/Cross-figure
% 
% William Y. Sit: "On Crossnumber Puzzles and The Lucas-Bonaccio Farm 1998
% http://scisun.sci.ccny.cuny.edu/~wyscc/CrossNumber.pdf
% 
% Bill Williams: Crossnumber Puzzle, The Little Pigley Farm
% http://jig.joelpomerantz.com/fun/dogsmead.html

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% This model was inspired by the ECLiPSe model written by Warwick Harvey:
% http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob021/code.html
%
%
% Data from 
% http://thinks.com/crosswords/xfig.htm.
%
% This problem is 001 from http://thinks.com/crosswords/xfig.htm 
% ("X" is the blackbox and is fixed to the value of 0)
%
% 1  2  3  4  5  6  7  8  9
% ---------------------------
% 1  2  _  3  X  4  _  5  6  % 1
% 7  _  X  8  _  _  X  9  _  % 2
% _  X  10 _  X  11 12 X  _  % 3
% 13 14 _  _  X  15 _  16 _  % 4 
% X  _  X  X  X  X  X  _  X  % 5 
% 17 _  18 19 X  20 21 _ 22  % 6
% _  X  23 _  X  24 _  X  _  % 7
% 25 26 X  27 _  _  X  28 _  % 8
% 29 _  _  _  X  30 _  _  _  % 9

%
% The answer is
%  1608 9183
%  60 201 42
%  3 72 14 1
%  5360 2866
%   3     4
%  4556 1156
%  9 67 16 8
%  68 804 48
%  1008 7332

% Solutions:
% MiniZinc and Gecode/fz solves the problem in about 8 seconds.
% ECLiPSe/ic: 35 seconds
% MiniZinc/fdmip in 14 seconds.
%


int: n = 9;
array[1..n, 1..n] of var 0..9: M;

set of int: D = 0..9999; % the max length of the numbers in this problem is 4
var D: A1;
var D: A4;
var D: A7;
var D: A8;
var D: A9;
var D: A10;
var D: A11;
var D: A13;
var D: A15;
var D: A17;
var D: A20;
var D: A23;
var D: A24;
var D: A25;
var D: A27;
var D: A28;
var D: A29;
var D: A30;

var D: D1;
var D: D2;
var D: D3;
var D: D4;
var D: D5;
var D: D6;
var D: D10;
var D: D12;
var D: D14;
var D: D16;
var D: D17;
var D: D18;
var D: D19;
var D: D20;
var D: D21;
var D: D22;
var D: D26;
var D: D28;


%
% across(Matrix, Across, Len, Row, Col)
%	Constrains 'Across' to be equal to the number represented by the
%	'Len' digits starting at position (Row, Col) of the array 'Matrix'
%	and proceeding across.
%
predicate across(array[int, int] of var D: Matrix, var D: Across, int: Len, int: Row, int: Col) =
   let {
     array[1..Len] of var D: tmp
   }
   in
   toNum10(tmp, Across)
   /\
   forall(i in 0..Len-1) (

       Matrix[Row,Col+i] = tmp[i+1]
   )
;

%
% down(Matrix, Down, Len, Row, Col):
%	Constrains 'Down' to be equal to the number represented by the
%	'Len' digits starting at position (Row, Col) of the array 'Matrix'
%	and proceeding down.
%
predicate down(array[int,int] of var D: Matrix, var D: Down, int: Len, int: Row, int: Col) =
   let {
     array[1..Len] of var D: tmp
   }
   in
   toNum10(tmp, Down)
   /\
   forall(i in 0..Len-1) (
      Matrix[Row+i,Col] = tmp[i+1]
   )
;


%
% converts a number <-> array
%
predicate toNum10(array[int] of var D: a, var D: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


%
% x is a square
% 
predicate square(var D: x) =
   exists(y in D) (
      y*y = x
   )
;


%
% very simple primality test
%
predicate is_prime(var int: x) =
   forall(i in 2..ceil(sqrt(9999.0))) ( 
        (i < x) -> (x mod i > 0)
   )
;


solve :: int_search(
         [M[i,j] | i,j in 1..n] ++
         [A1,A4,A7,A8,A9,A10,A11,A13,A15,A17,A20,A23,A24,A25,A27,A28,A29,A30,
         D1,D2,D3,D4,D5,D6,D10,D12,D14,D16,D17,D18,D19,D20,D21,D22,D26,D28],
         occurrence,
         indomain_min,
         complete
         ) 
     satisfy;


constraint

   % Set up the constraints between the matrix elements and the
   % clue numbers.
   across(M, A1, 4, 1, 1)  /\ 
   across(M, A4, 4, 1, 6)  /\ 
   across(M, A7, 2, 2, 1)  /\ 
   across(M, A8, 3, 2, 4)  /\ 
   across(M, A9, 2, 2, 8)  /\ 
   across(M, A10, 2, 3, 3) /\ 
   across(M, A11, 2, 3, 6) /\ 
   across(M, A13, 4, 4, 1) /\ 
   across(M, A15, 4, 4, 6) /\ 
   across(M, A17, 4, 6, 1) /\ 
   across(M, A20, 4, 6, 6) /\ 
   across(M, A23, 2, 7, 3) /\ 
   across(M, A24, 2, 7, 6) /\ 
   across(M, A25, 2, 8, 1) /\ 
   across(M, A27, 3, 8, 4) /\ 
   across(M, A28, 2, 8, 8) /\ 
   across(M, A29, 4, 9, 1) /\ 
   across(M, A30, 4, 9, 6) /\ 

   down(M, D1, 4, 1, 1)  /\ 
   down(M, D2, 2, 1, 2)  /\ 
   down(M, D3, 4, 1, 4)  /\ 
   down(M, D4, 4, 1, 6)  /\ 
   down(M, D5, 2, 1, 8)  /\ 
   down(M, D6, 4, 1, 9)  /\ 
   down(M, D10, 2, 3, 3) /\ 
   down(M, D12, 2, 3, 7) /\ 
   down(M, D14, 3, 4, 2) /\ 
   down(M, D16, 3, 4, 8) /\ 
   down(M, D17, 4, 6, 1) /\ 
   down(M, D18, 2, 6, 3) /\ 
   down(M, D19, 4, 6, 4) /\ 
   down(M, D20, 4, 6, 6) /\ 
   down(M, D21, 2, 6, 7) /\ 
   down(M, D22, 4, 6, 9) /\ 
   down(M, D26, 2, 8, 2) /\ 
   down(M, D28, 2, 8, 8) /\ 


   % Set up the clue constraints.
%  Across
%  1 27 across times two
%  4 4 down plus seventy-one
%  7 18 down plus four
%  8 6 down divided by sixteen
%  9 2 down minus eighteen
% 10 Dozen in six gross
% 11 5 down minus seventy
% 13 26 down times 23 across
% 15 6 down minus 350
% 17 25 across times 23 across
% 20 A square number
% 23 A prime number
% 24 A square number
% 25 20 across divided by seventeen
% 27 6 down divided by four
% 28 Four dozen
% 29 Seven gross
% 30 22 down plus 450 

   A1 = 2 * A27         /\ 
   A4 = D4 + 71         /\ 
   A7 = D18 + 4         /\ 
   A8 = D6 div 16       /\ 
   A9 = D2 - 18         /\ 
   A10 = 6 * 144 div 12 /\ 
   A11 = D5 - 70        /\ 
   A13 = D26 * A23      /\ 
   A15 = D6 - 350       /\ 
   A17 = A25 * A23      /\ 
   square(A20)          /\ 
   is_prime(A23)        /\
   square(A24)          /\ 
   A25 = A20 div 17     /\ 
   A27 = D6 div 4       /\ 
   A28 = 4 * 12         /\ 
   A29 = 7 * 144        /\ 
   A30 = D22 + 450      /\ 

   % Down
   %
   %  1 1 across plus twenty-seven
   %  2 Five dozen
   %  3 30 across plus 888
   %  4 Two times 17 across
   %  5 29 across divided by twelve
   %  6 28 across times 23 across
   % 10 10 across plus four
   % 12 Three times 24 across
   % 14 13 across divided by sixteen
   % 16 28 down times fifteen
   % 17 13 across minus 399
   % 18 29 across divided by eighteen
   % 19 22 down minus ninety-four
   % 20 20 across minus nine
   % 21 25 across minus fifty-two
   % 22 20 down times six
   % 26 Five times 24 across
   % 28 21 down plus twenty-seven 

   D1 = A1 + 27     /\ 
   D2 = 5 * 12      /\ 
   D3 = A30 + 888   /\ 
   D4 = 2 * A17     /\ 
   D5 = A29 div 12  /\ 
   D6 = A28 * A23   /\ 
   D10 = A10 + 4    /\ 
   D12 = A24 * 3    /\ 
   D14 = A13 div 16 /\ 
   D16 = 15 * D28   /\ 
   D17 = A13 - 399  /\ 
   D18 = A29 div 18 /\ 
   D19 = D22 - 94   /\ 
   D20 = A20 - 9    /\ 
   D21 = A25 - 52   /\ 
   D22 = 6 * D20    /\ 
   D26 = 5 * A24    /\ 
   D28 = D21 + 27


   % Fix the blackboxes
   /\
   M[1,5] = 0 /\
   M[2,3] = 0 /\
   M[2,7] = 0 /\
   M[3,2] = 0 /\
   M[3,5] = 0 /\
   M[3,8] = 0 /\
   M[4,5] = 0 /\
   M[5,1] = 0 /\
   M[5,3] = 0 /\
   M[5,4] = 0 /\
   M[5,5] = 0 /\
   M[5,6] = 0 /\
   M[5,7] = 0 /\
   M[5,9] = 0 /\
   M[6,5] = 0 /\
   M[7,2] = 0 /\
   M[7,5] = 0 /\
   M[7,8] = 0 /\
   M[8,3] = 0 /\
   M[8,7] = 0 /\
   M[9,5] = 0
;


output [
 show([A1,A4,A7,A8,A9,A10,A11,A13,A15,A17,A20,A23,A24,A25,A27,A28,A29,A30,
       D1,D2,D3,D4,D5,D6,D10,D12,D14,D16,D17,D18,D19,D20,D21,D22,D26,D28]), "\n",
] ++ 
[
  if j = 1 then "\n" else " " endif ++
    show(M[i,j])
  | i,j  in 1..n
] ++ ["\n"];