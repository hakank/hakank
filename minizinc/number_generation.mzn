% 
% Number generation in MiniZinc.
%
% Generate a sum with some operators.
%
% Cf devils_word.mzn
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

%int: n = 4;
%array[1..n] of var 1..n*2: x;
%array[1..n-1] of var 1..4: ops; % array of operations

% for simulating Devil's Word:
int: n = 19;
array[1..n] of var 1..300: x;
array[1..n-1] of var 1..2: ops; % array of operations

var 0..1000: total;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain_min", "complete") satisfy;

%
%  res = a op b
%  
predicate make_op(var int: a, var int: b, var int: xop, var int: res) =
  (xop = 1 <-> res = a + b)
  /\
  (xop = 2 <-> res = a - b)
  /\
  (xop = 3 <-> res = a * b)
  /\
  (xop = 4 <-> res = a div b)
%  /\
%  (xop = 5 <-> res = a * a + b* b)
%  /\ % note: Using this alternative may generate weird results
%  (xop = 6 <-> res = a mod b)
;


%
%
%
predicate make(array[int] of var int: y, array[int] of var 1..4: xop, var int: res) =
   let {
     array[1..length(y)-1] of var int: s % temp array of the result of operation
   }
   in
   forall(i in 1..length(y)-1) (
     make_op(y[i], y[i+1], xop[i], s[i])
   )
   /\
   res = sum(s)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint

   % % cf http://en.wikibooks.org/wiki/Puzzles/Arithmetical_puzzles/Four_4s_Equal...
   % x = [4,4,4,4]
   % /\

   % total = 123

   make(x, ops, total)
   
   /\ % This is the Devil's word example
   total = 666
   /\ % [h,å,k,a,n," ", k,j,e,l,le,r,s,t,r,a,n,d]
   cp1d(x, [72, 229, 107, 97, 110, 32, 75, 106, 101, 108, 108, 101, 114, 115, 116, 114, 97, 110, 100])

;


output [
  "total: ", show(total), "\n",
  "x: ", show(x), "\n",
  "ops: ", show(ops), "\n"

];


%
% data
%
% n = 4;
