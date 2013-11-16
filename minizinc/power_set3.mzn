% 
% Powerset as a predicate in MiniZinc.
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;
int: m = pow(2,n)-1;

% decision variables
array[1..m, 1..n] of var 0..1: ps1; % 0..1

array[1..m, 1..n] of var 0..n: ps2; % 0..n


predicate toNum(array[int] of var int: a, var int: n, int: base) =
    let { int: len = length(a) }
    in
    n = sum(i in 1..len) (
      pow(base, len-i) * a[i]
    )
;

predicate powerset(int: s, array[int,int] of var int: p) =
  let {
    int: mm = pow(2,s)-1
  } in
   forall(i in 1..mm) (
      toNum([p[i,j] | j in 1..s], i, 2)
   )
;

%
% powerset as a matrix of 0..n
%
predicate powerset2(int: s, array[int,int] of var int: p) =
  let {
    int: mm = pow(2,s)-1,
    array[1..mm, 1..s] of var 0..1: ps
  } in
  powerset(s,ps)
  /\
  forall(i in 1..mm) (
     forall(j in 1..s) (
        p[i,j] = (s-j+1)*ps[i,j]
     )
  )
;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   powerset(n, ps1) /\
   powerset2(n, ps2)
;

output 
[
  if j = 1 then "\n" else " " endif ++
    show(ps1[i,j])
  | i in 1..m, j in 1..n
]
++
[ "\n"] 
++
[
  if j = 1 then "\n" else " " endif ++
    show(ps2[i,j])
  | i in 1..m, j in 1..n
]
;

