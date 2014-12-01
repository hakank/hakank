% 
% Mislabeled boxes problem in MiniZinc.
% 
% This is a classic theorem proving problem.
%
% From http://www.ub-net.de/cms/proverbox-ex-boxes.html
% """
% File     : THIS SPECIFICATION IS A VARIANT OF THE FOLLOWING TPTP PROBLEM:
%            PUZ012-1 : TPTP v2.5.0. Bugfixed v1.2.1.
% Domain   : Puzzles
% Problem  : The Mislabeled Boxes
% Version  : Especial.
% English  : There are three boxes a, b, and c on a table. Each box contains
%            apples or bananas or oranges. No two boxes contain the same
%            thing. Each box has a label that says it contains apples or says 
%            it contains bananas or says it contains oranges. No box contains
%            what it says on its label. The label on box a says "apples". 
%            The label on box b says "oranges". The label on box c says
%            "bananas". You pick up box b and it contains apples. What do
%            the other two boxes contain?

% Refs     : [WO+92] Wos et al. (1992), Automated Reasoning: Introduction a
%          : [Wos88] Wos (1988), Automated Reasoning - 33 Basic Research Pr
% Source   : [ANL]
% Names    : Boxes-of-fruit [WO+92]
%          : Boxes-of-fruit [Wos88]
%          : boxes.ver1.in [ANL]
% """
%
% Also see TPTP, Thousands of Problems for Theorem Provers)
% http://www.cs.miami.edu/~tptp/
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 3;

% boxes
int: a = 1;
int: b = 2;
int: c = 3;

% labels and the fruits
int: apples = 1;
int: oranges = 2;
int: bananas = 3;
% The label on box a says "apples". The label on box b says "oranges". 
% The label on box c says "bananas". 
array[1..n] of int: labels = [apples,oranges,bananas];

% what does the boxes contain
array[1..n] of var 1..n: x;

solve satisfy;

predicate has_fixpoint(array[int] of var int: a) =
  exists(i in index_set(a)) (
      a[i] = i
  )
;

constraint

  all_different(x) 

  % No box contains what it says on its label. 
  % /\
  % forall(i in 1..n) (
  %    x[i] != labels[i]
  % )
  /\ not(has_fixpoint(x))

  % You pick up box b and it contains apples. 
  /\ x[b] = apples

  % What do the other two boxes contain?
;

output [
  show(x)
];

