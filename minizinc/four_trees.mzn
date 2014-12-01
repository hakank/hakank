% 
% Four trees in MiniZinc.
% 
% http://l4f.cecs.anu.edu.au/puzzles/beginner/four-trees
% """
% Four trees are planted at the corners of a square. 
% They are an oak, an elm, an ash and a fir. 
% I walk around three sides of the square, thus passing all four trees once.
% 
% I notice that the oak and ash are not adjacent. 
% I passed the fir before either the elm or the oak, 
% and I neither started nor finished at the ash.
% 
% In what order did I pass the trees?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;

% decision variables
var 1..n: oak;
var 1..n: elm;
var 1..n: ash;
var 1..n: fir;

array[1..n] of string: s = ["oak","elm","ash","fir"];

array[1..n] of var 1..n: position = [oak,elm,ash,fir];
array[1..n] of var 1..n: order;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(position) /\

  % I notice that the oak and ash are not adjacent. 
  abs((oak mod (n+1)) - (ash mod (n+1))) > 1 /\

  % I passed the fir before either the elm or the oak, 
  (fir < elm /\ fir < oak) /\

  % and I neither started nor finished at the ash.
  ash > 1 /\ ash < n
  
  /\ inverse(position,order)
;

output [

  "position: ", show(position), "\n",
  "order   : ", show(order), "\n",
  "order2  : ", show(join(", ", [s[fix(order[i])] | i in 1..n])), "\n"

];

