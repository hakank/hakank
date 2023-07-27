% 
% Global constraint among_seq in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Camong_seq.html
% """
% Constrains all sequences of SEQ consecutive variables of the collection 
% VARIABLES to take at least LOW values in VALUES and at most 
% UP values in VALUES.
% 
% Example
% (
% 1, 2, 4, <9, 2, 4, 5, 5, 7, 2>,
%          <0, 2, 4, 6, 8>
% )
% 
% The among_seq constraint holds since the different sequences of 4 
% consecutive variables contains respectively 2, 2, 1 and 1 even numbers.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 7;
int: m = 5;
array[1..n] of var 1..9: x;
array[1..m] of var 0..8: y;
var int: low;
var int: up;
int: seq = 4;


predicate among_seq(var int: low, var int: up, int: seq, array[int] of var int: x, array[int] of var int: y)=
   let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
   }
   in
  forall(i in lbx..n-seq+1) (
      let {
        var int: s = 
            sum(j in i..i+seq-1) ( 
               sum(k in index_set(y)) (
                 bool2int(x[j] = y[k])
               )
            )
      }
      in
      s >= low
      /\
      s <= up
  )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


solve satisfy;

constraint
  cp1d([9, 2, 4, 5, 5, 7, 2], x)
  /\
  cp1d([0,2,4,6,8], y)
  /\ 
  low = 1
  /\ 
  up = 2
  /\
  among_seq(low, up, seq, x, y)
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "low: " ++ show(low) ++ "\n" ++
  "up: " ++ show(up) ++ "\n" ++ 
  "y: " ++ show(y) 
];
