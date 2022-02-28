/* 

  Permutation3 in MiniZinc.

  This model defined two predicate which ensures that array b
  is a permutation of array a, given in the permutation array p.

  * permutation3(a,p,b)
    Requires an extra var array for the permutation. 
  
  * permutation3b(a,b)
    This version has the permuutation inside the predicate
    instead.

  Cf https://stackoverflow.com/questions/70984019/minizinc-constraint-to-enforce-two-arrays-be-equal

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/

include "globals.mzn"; 

int: n = 3;

array[1..n] of var 1..2: x;
array[1..n] of var 1..2: y;
array[1..n] of var 1..n: p; % Comment this if using permutation3b

/*
  Enforce that array b is a permutation of array a with the
  permutation array p.
*/
predicate permutation3(array[int] of var int: a,
                       array[int] of var int: p, 
                       array[int] of var int: b) =
  forall(i in index_set(a)) (
    b[i] = a[p[i]]
  )
  /\
  all_different(p)
;

/*
  Enforce that array b is a permutation of array a.
*/
predicate permutation3b(array[int] of var int: a,
                       array[int] of var int: b) =
  let {
    array[index_set(a)] of var index_set(a): p;
  } in
  forall(i in index_set(a)) (
    b[i] = a[p[i]]
  )
  /\
  all_different(p)
;
                   
solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  x = [2,2,1] /\
  permutation3(x,p,y)
  % permutation3b(x,y)
;

output [
  "x: \(x)\ny: \(y)\np: \(p)\n"
  % "x: \(x)\ny: \(y)\n\n"
];

