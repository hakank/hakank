% 
% Global constraint inflexions in MiniZinc.
% 
% Global constraint catalogue
% https://sofdem.github.io/gccat/gccat/Cinflexions.html
% """
% N is equal to the number of times that the following conjunctions of 
% constraints hold:
%
%  * Xi CTR Xi+1 /\ Xi != Xi+1,
%  * Xi+1 = Xi+2 /\.../\ Xj-2 = Xj-1,
%  * Xj-1 != Xj /\ Xj-1 not CTRX j.
% 
% where Xk is the kth item of the VARIABLES collection and 
% 1 <= i, i+2 <= j, j <= n 
% and CTR is < or >​.
%
% Example:
% (3, <var-1, var-1, var-4, var-8, var-8, var-2, var-7, var-1>)
% 
% The inflexion constraint holds since the sequence 1 1 4 8 8 2 7 1 
% contains three inflexions peaks that respectively correspond to values 
% 8, 2 and 7.
% """
%
% Cf the Sicstus Prolog doc where an automaton is used:
% http://www.sics.se/sicstus/docs/4.0.3/html/sicstus/Combinatorial-Constraints.html
%

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 8;
array[1..n] of var 1..8: x;
var int: a;

solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

% 
% inflexions
%
predicate inflexions(var int: a, array[int] of var int: x) = 
  let {
    int: lbx = min(index_set(x)),
    int: ubx = max(index_set(x)),
  }
  in
  a = sum(i in lbx..ubx, j in i+2..ubx) (
    bool2int(
      forall(k in i+2..j-1) ( x[k-1] = x[k] )
      /\ % CTR is either "<" or ">"
      ( 
        ( % < and not(<)
         (x[i] < x[i+1] /\ x[i] != x[i+1])
         /\
         (x[j-1] != x[j] /\ not(x[j-1] < x[j]))
        )
        \/
        ( % > and not(>)
         (x[i] > x[i+1] /\ x[i] != x[i+1])
         /\
         (x[j-1] != x[j] /\ not(x[j-1] > x[j]))
        )
      )

     ) % end bool2int
  ) % end sum
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint
  cp1d(x, [1,1,4,8,8,2,7,1])
  /\
%  cp1d(x, [2,3,_,_,_,_,_,_])
%  /\
  inflexions(a, x)
%  /\
%  a = 5

;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "a: " ++ show(a) ++ "\n"
];
