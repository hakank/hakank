% 
% Evens puzzle in MiniZinc.
%
% From Model example k4even_ka.mos
% """
% Placing a given number of chips on a 4x4 grid 
% such that every row and every column contains
% an even number of chips.
% """ 

%
% Compare with http://www.hakank.org/minizinc/evens.mzn
%

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s4.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: NPOS = 4;
set of int: POS = 1..NPOS; % Positions is a row/column 
int: NCHIP = 10;           % Number of chips

% decision variables
array[POS,POS] of var 0..1: place; % 1 if chip at the position, 0 otherwise

solve satisfy;
  
constraint

  %  Total number of chips
  sum(r,c in POS) (place[r,c]) = NCHIP

  /\
  % Even number of chips in all rows and columns
  forall(i in POS) (
     sum(j in POS) (place[i,j]) mod 2 = 0
     /\
     sum(j in POS) (place[j,i]) mod 2 = 0
  )
;

output 
[
   if j = 1 then "\n" else " " endif ++
     show(place[i,j])
   | i, j in POS
] ++ ["\n"];