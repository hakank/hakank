% 
% Quasigroup completion problem in MiniZinc.
% 
% See 
% Carla P. Gomes and David Shmoys:
% "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
%
% See demo applet by Carla P. Gomes
% http://www.cs.cornell.edu/gomes/QUASIdemo.html
  
% See also
% Ivars Peterson "Completing Latin Squares"
% http://www.maa.org/mathland/mathtrek_5_8_00.html
% """
% Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
% a four-by-four array so that no column or row contains the same two numbers. 
% The result is known as a Latin square.
% ...
% The so-called quasigroup completion problem concerns a table that is correctly 
% but only partially filled in. The question is whether the remaining blanks in 
% the table can be filled in to obtain a complete Latin square (or a proper 
% quasigroup multiplication table).
% """
% 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "quasigroup_completion_model.mzn"; 

n = 4;

constraint
    % Example from Gomes & Shmoys, page 3.
    % Solution:
    % 4 1 2 3
    % 2 3 4 1
    % 1 4 3 2
    % 3 2 1 4
    cp2d(x, array2d(1..n, 1..n, 
    [
     _, 1, 2, 3,
     2, _, 4, 1, 
     1, 4, _, 2,
     3, _, 1, _
    ]))
    /\
    k_all_different(x)

;

