% 
% Quasigroup completion problem in MiniZinc.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "quasigroup_completion_model.mzn"; 

n = 5;

constraint
    % Example from Ruben Martins and Inès Lynce
    % Breaking Local Symmetries in Quasigroup Completion Problems, page 3
    % The solution is unique:
    % 1 3 2 5 4
    % 2 5 4 1 3
    % 4 1 3 2 5
    % 5 4 1 3 2
    % 3 2 5 4 1
    cp2d(x, array2d(1..n, 1..n, 
     [
      1, _, _, _, 4,
      _, 5, _, _, _,
      4, _, _, 2, _,
      _, 4, _, _, _,
      _, _, 5, _, 1
    ]))
;
