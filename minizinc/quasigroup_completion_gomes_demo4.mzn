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
    % Pattern #4. 
    %  dark red   = 1
    %  light blue = 2
    %  dark blue  = 3
    %  light red  = 4
    % Note: There are no solutions to this problem.
    cp2d(x, array2d(1..n, 1..n, 
    [
      1,_,_,_,_,_,_,_,_,_,
      2,1,_,_,_,_,_,_,_,4,
      3,2,1,_,_,_,_,_,4,_,
      _,3,2,1,_,_,_,4,_,_,
      _,_,3,2,1,_,4,_,_,_,
      _,_,_,3,2,1,_,_,_,_,
      _,_,_,_,3,2,1,_,_,_,
      _,_,_,4,_,3,2,1,_,_,
      _,_,4,_,_,_,3,2,1,_,
      _,4,_,_,_,_,_,3,2,1,
    ]))

;

