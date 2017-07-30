% 
% Solve the equation in MiniZinc.
% 
% 11x11=4 
% 22x22=16 
% 33x33=?
% 
% This model solves the problem with four interpretations.
% 
% (2013-03-11: I've seen this problem in my web server log the 
%              last days. Don't know the origin.)
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

var int: x1;
var int: x2;
var int: x3;
var int: x4;

solve satisfy;

predicate toNum(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
;

predicate digit_sum(array[int] of var int: a, var int: v) =
   let {
     array[1..2] of var 0..99: t
   }
   in  
   toNum(t, sum(a)) /\
   v = sum(t)
;


% This is perhaps the most natural interpretation.
predicate interpretation1(var int: x) =
    (1+1) * (1+1) = 4 /\
    (2+2) * (2+2) = 16 /\
    (3+3) * (3+3) = x
;


% Alternative solution using digit sums.
predicate interpretation2(var int: x) =
   let {
     int: n = 6,
     array[1..n] of var 0..9: a,
     array[1..n] of var 0..9: b,
     array[1..n] of var 0..9: c
   } in
    toNum(a, 11*11) /\ sum(a) = 4 /\
    toNum(b, 22*22) /\ sum(b) = 16 /\
    toNum(c, 33*33) /\ sum(c) = x
;


% helper predicate for interpretation3:
%    4^i
predicate s3(int: i, var int: y) =
   ceil(pow(4.0,int2float(i))) = y
;

predicate interpretation3(var int: x) =
    s3(1, 4) /\ 
    s3(2, 16) /\
    s3(3, x)
;

% 4*i^i
predicate s4(int: i, var int: y) =
   let {
     float: f = int2float(i),
     int: t= ceil(pow(f,f))
   } in
   y = 4*t
;

predicate interpretation4(var int: x) =
    s4(1, 4) /\ 
    s4(2, 16) /\
    s4(3, x)
;

constraint
   interpretation1(x1) /\
   interpretation2(x2) /\
   interpretation3(x3) /\
   interpretation4(x4) 
;


output [
  "x1: " ++ show(x1) ++ "\n" ++
  "x2: " ++ show(x2) ++ "\n" ++
  "x3: " ++ show(x3) ++ "\n" ++
  "x4: " ++ show(x4) ++ "\n"
];
