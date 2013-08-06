%
% Contracting Costs puzzle in Minizinc.
%
% From http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
% """
% Contracting Costs    from "Mathematical Puzzles of Sam Loyd, Volume 2",
% number 20.
%
% A contractor planning the construction of a house found that he would
% have to pay:
% 
%    * $ 1,100 to the paper hanger and the painter,
%    * $ 1,700 to the painter and plumber,
%    * $ 1,100 to the plumber and electrician,
%    * $ 3,300 to the electrician and carpenter,
%    * $ 5,300 to the carpenter and mason,
%    * $ 3,200 to the mason and painter.
%
%  What does each man charge for his services?
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: m = 5300;
var 0..m: Ph;
var 0..m: Pa;
var 0..m: Pl;
var 0..m: El;
var 0..m: Ca;
var 0..m: Ma;

solve minimize Ph+Pa+Pl+El+Ca+Ma+Pa;
% solve satisfy;

constraint
  1100 = Ph + Pa
  /\ 1700 = Pa + Pl
  /\ 1100 = Pl + El
  /\ 3300 = El + Ca
  /\ 5300 = Ca + Ma
  /\ 3200 = Ma + Pa
;

output [
 "Ph:", show(Ph), " ",
 "Pa:", show(Pa), " ",
 "Pl:", show(Pl), " ",
 "El:", show(El), " ",
 "Ca:", show(Ca), " ",
 "Ma:", show(Ma), " ", "\n",
]
