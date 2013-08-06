% 
% Five floors problem in MiniZinc.
% 
% From Alexey Radul & Gerald Jay Sussman:
% "The Art of Propagator", page 34
% """
% Baker, Cooper, Fletcher, Miller, and Smith live on the first
% five floors of this apartment house. Baker does not live on the
% fifth floor. Cooper does not live on the first floor. Fletcher
% does not live on either the fifth or the first floor. Miller lives
% on a higher floor than does Cooper. Smith does not live on a
% floor adjacent to Fletcher'. Fletcher does not live on a floor
% adjacent to Cooper's.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;

var 1..n: Baker;
var 1..n: Cooper;
var 1..n: Fletcher;
var 1..n: Miller;
var 1..n: Smith;


array[1..n] of var 1..n: x = [Baker, Cooper, Fletcher, Miller, Smith];


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  alldifferent(x)
;

constraint
    % Baker does not live on the fifth floor.
    Baker != 5

    % Cooper does not live on the first floor. 
    /\
    Cooper != 1

    % Fletcher does not live on either the fifth or the first floor. 
    /\
    (Fletcher != 5 /\ Fletcher != 1)

    % Miller lives on a higher floor than does Cooper. 
    /\
    Miller > Cooper

    % Smith does not live on a floor adjacent to Fletcher'. 
    /\
    abs(Smith-Fletcher) > 1

    % Fletcher does not live on a floor adjacent to Cooper's.
    /\
    abs(Fletcher-Cooper) > 1
;


output [
  "x" ++ show(x)
]
 ++ ["\n"]
;

