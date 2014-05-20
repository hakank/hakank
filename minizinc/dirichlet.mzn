% 
% Triangle problem in MiniZinc.
% 
% From the lecture notes Modelling with Constraints
% http://www.cse.unsw.edu.au/~cs4418/2008/Lectures/Modelling2.ppt, page 45ff

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n;
array[1..n, 1..n] of var 0.0..100.0: temp;

constraint
  forall(i,j in 2..n-1) (
  temp[i,j] = 1.0/4.0 *
     (temp[i-1,j] + temp[i+1,j] + temp[i,j-1] + temp[i,j+1])
);


solve satisfy;
% solve :: float_search([temp[i,j] | i,j in 1..n],0.0000001,input_order, indomain_split, complete) satisfy;

int: half_n = n div 2;

output [
  if j = 1 then "\n" else " " endif ++
    show(temp[i,j])
  | i,j in 1..n
]
++
[
  "\nTemperature at (", show(half_n), ", ", show(half_n), ") is ",
   show(temp[half_n, half_n])
]
;

%
% data
%
n = 7;

temp = array2d(1..n, 1..n, 
  [
    100.0,100.0,100.0,100.0,100.0,100.0,100.0,
      0.0,    _,    _,    _,    _,    _,  0.0,
      0.0,    _,    _,    _,    _,    _,  0.0,
      0.0,    _,    _,    _,    _,    _,  0.0,
      0.0,    _,    _,    _,    _,    _,  0.0,
      0.0,    _,    _,    _,    _,    _,  0.0,
      0.0,    _,    _,    _,    _,    _,  0.0]);