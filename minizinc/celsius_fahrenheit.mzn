% 
% Conversion Celsius <-> Fahrenheit in MiniZinc.
% 
% This is a standard example in constraint programming showing the
% "double direction": i.e. a single predicate can convert from Celsius to 
% Fahrenheit or from Fahrenheit to Celsius.
% 


% Note: As of writing (2008-12-08 using the latest ROTD per 2008-09-24) 
% not all solvers handle var floats correctly.
%
% The following solvers works:
%  - MiniZinc/mip
%  - MiniZinc/fdmip
%  - ECLiPSe ic
%  - ECLiPSe eplex
%  - Gecode/fz
%
% The following solver don't work with this model:
%  - MiniZinc/flatzinc (standard fd solver)
%  - ECLiPSe/fd
%  - Tini fz
%  

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var -300.0..300.00: C; % Celsius
var -300.0..300.00: F; % Fahrenheigh

predicate convert(var float: C, var float: F) =
  C = (F - 32.0) * 5.0 / 9.0
;

% solve satisfy;
solve :: float_search([C,F],0.000001,input_order,indomain_split, complete) satisfy;

constraint

%  F = 100.0
%  /\
  C = 37.0
  /\
  convert(C, F)
%  /\
%  C = F

  /\ (C > 1.0 \/ F > 1.0)

  % /\ % either C or F must be an integer
  % floor(F) = F  % floor don't accept var float

;


output [
  "C: ", show(C), "\n",
  "F: ", show(F), "\n",

];