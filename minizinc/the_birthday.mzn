% 
% The Birthday problem in MiniZinc.
% 
% From http://www.mcs.vuw.ac.nz/courses/OPRE251/2006T1/Labs/lab12.pdf
% """
% The Birthday
% Frank is hurrying home late, after a particularly grueling day, when it pops 
% into his mind that today is Kitty's birthday! Or is it? Everything is closed 
% except the florist. 
% If it is not her birthday and he brings no gift, the situation will be neutral,
% i.e., payoff 0. If it is not her birthday and he comes in bursting with roses,
% and obviously confused, she may be very suspicious, a payoff of say -1. If it 
% is her birthday and he has, clearly, remembered it, that is worth say 2. If 
% he has forgotten it, he is down like a stone, say, -10. So Frank mentally 
% forms the payoff matrix:
%
%                      Nature
%
%                   Not Birthday      Birthday
%         Nothing      0                -10
% Frank
%         Flowers     -1                 2
%
% """

% Result: 
%  x[1] (Nothing) yield 0.230769230769231 
%  x[2] (Flowers) yield 0.769230769230769 är is - of course - the way to do.
%


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

array[1..2,1..2] of int: m = 
  array2d(1..2,1..2,
    [
     %             Nature
     % Not Birthday    Birthday 
         0,             -10,   % Frank: Nothing
        -1,               2,   % Frank: Flowers
    ]
  );  


% decision variables
array[1..2] of var 0.0..1.0: x;
var -1.0..1.0: v;

solve :: float_search(x ++ [v],0.0001, first_fail, indomain_split, complete) maximize v;

constraint
  forall(j in 1..2) (
     v <= sum([m[i,j]*x[i] | i in 1..2]) 
  ) 
  /\
  sum(x) = 1.0
;

output [
  "x[1]:\(x[1]) x[2]:\(x[2]) v: \(v)"
];

