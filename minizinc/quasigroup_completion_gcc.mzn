% 
% Quasigroup completion problem in MiniZinc.
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "quasigroup_completion_model.mzn"; 

n = 4;

constraint
    % Example from Global Constraint Catalogue
    % http://www.emn.fr/x-info/sdemasse/gccat/Klatin_square.html
    %
    % 12 solutions.
    cp2d(x, array2d(1..n, 1..n, 
    [
       1, _, _, _,
       _, _, _, 3,
       3, _, _, _,
       _, _, _, 1
    ]))
;

