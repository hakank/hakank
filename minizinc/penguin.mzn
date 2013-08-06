% 
% Penguin problem (nonmonotonic reasoning) in MiniZinc.
% 
% From the Alma-0 model penguin.a0
% (http://www.cwi.nl/en/alma)
% """
% This program shows that it is possible to use Alma-0 for 
% nonmonotonic reasoning.
% We stipulate the following assumptions.
%
% - The birds which are not abnormal fly
% (i.e., birds usually fly).
% - The penguins are abnormal.
% - Penguins and eagles are birds.
% - Tweety is a penguin and Toto is an eagle.
%
% The problem is to deduce which of these two birds flie
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Answer: 2, i.e. Toto is a bird that can fly. Tweety isn't.
% 

int: Tweety = 1;
int: Toto = 2;

set of int: Bird = Tweety..Toto;
% var Bird: x;
array[1..2] of Bird: birds = [Tweety, Toto];

array[Bird] of var 0..1: flies; % which flies?

predicate penguin(var Bird: x) =
  x = Tweety
;

predicate eagle(var Bird: x) = 
  x = Toto
;

predicate ab(var Bird: x) = 
  penguin(x)	
;

predicate bird(var Bird: x) = 
  penguin(x) 
  \/ 
  eagle(x)
;


predicate fly(var Bird: x) = 
  bird(x) 
  /\ 
  not(ab(x))
;

predicate does_fly(var Bird: x, var 0..1: f) = 
  f = 1 <-> (bird(x) /\ not(ab(x)))
;


solve satisfy;

constraint
   % fly(x)
   forall(t in Bird) (
     does_fly(t, flies[t])
   )
;

output
[
  show(flies)
];