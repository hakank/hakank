% 
% Global constraint int_value_precede in MiniZinc.
%  
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cint_value_precede.html
% """
% If value T occurs in the collection of variables VARIABLES then its first 
% occurrence should be preceded by an occurrence of value S.
%
% Example
%    ​(0,1, <4, 0, 6, 1, 0>
%

% Cf the more general constraint precedence.mzn.
% 



% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 0..6: x;

%
% precedence
% 
% Note: This implementation is different from Walsh's suggestion.
%
predicate int_value_precede(var int: s, var int: t, array[int] of var int: x) =
    let {
      int: lbx = min(index_set(x)),
      int: ubx = max(index_set(x)),
      var lbx..ubx: s_pos,
      var lbx..ubx: t_pos
    } 
    in
    ( % no t?  
      sum(i in lbx..ubx) (bool2int(x[i] = t)) 
        = 0
       /\
        (s_pos = 1 /\ t_pos = 1) % dummy values
    )
    \/
    ( % there is a t
     s_pos < t_pos
     /\
     x[s_pos] = s
     /\
     x[t_pos] = t
     /\ 
     forall(i in lbx..ubx) (
       (i <= x[s_pos]) -> x[i] != t
     )
    )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint

  cp1d(x, [4, 0, 6, 1, 0])
  % x = [0,2,3,0,2]  % no t
  % x = [4, 1, 6, 0, 0] % constraint don't hold
  /\
  int_value_precede(0, 1, x)
  

;

output
[
  show(x)
];
