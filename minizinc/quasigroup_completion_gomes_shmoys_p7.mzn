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
    % Example from Gomes & Shmoys, page 7
    % Two solutions.
    cp2d(x, array2d(1..n, 1..n, 
     [
      _, 1, _, _,
      _, _, 2, _,
      _, 3, _, _,
      _, _, _, 4
    ]))
;

