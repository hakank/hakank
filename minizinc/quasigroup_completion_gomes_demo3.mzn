% 
% Quasigroup completion problem in MiniZinc.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "quasigroup_completion_model.mzn"; 

n = 10;

constraint

    % Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
    % (n = 10)
    % Pattern #3. 
    % Coding:
    %    dark red   = 1
    %    light blue = 2 
    %    dark blue  = 3 
    %    light red  = 4
    %    brown      = 5
    %    green      = 6
    %    pink       = 7
    %    grey       = 8
    %    black      = 9
    %    yellow     = 10    
    % There are 40944 solutions for this pattern.
    cp2d(x, array2d(1..n, 1..n, 
    [
      _, _, 1, 5, 2, 6, 7, 8, _, _,
      _, 1, 5, 2, _, _, 6, 7, 8, _,
      1, 5, 2, _, _, _, _, 6, 7, 8,
      5, 2, _, _, _, _, _, _, 6, 7,
      2, _, _, _, _, _, _, _, _, 6,
      4,10, _, _, _, _, _, _, 3, 9,
      _, 4,10, _, _, _, _, 3, 9, _,
      _, _, 4,10, _, _, 3, 9, _, _,
      _, _, _, 4,10, 3, 9, _, _, _, 
      _, _, _, _, 4, 9, _, _, _, _
    ]))

;
