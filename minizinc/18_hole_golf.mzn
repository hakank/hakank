% 
% Can you Golf Golf? in MiniZinc.
% 
% http://codegolf.stackexchange.com/questions/8429/can-you-golf-golf/
% """
% You are required to generate a random 18-hole golf course.
%
% Example output:
% 
% [3 4 3 5 5 4 4 4 5 3 3 4 4 3 4 5 5 4]
% 
% Rules:
% 
%   - Your program must output a list of hole lengths for exactly 18 holes
%   - Each hole must have a length of 3, 4 or 5
%   - The hole lengths must add up to 72 for the entire course
%   - Your program must be able to produce every possible hole configuration with 
%     some non-zero-probability (the probabilities of each configuration need not be 
%     equal, but feel free to claim extra kudos if this is the case)
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

%
% golf version (76 chars)
%
array[1..18]of var 3..5:x;solve satisfy;constraint sum(x)=72;output[show(x)];

% Not so golf
% include "globals.mzn"; % for increasing
%
% array[1..18] of var 3..5: x;
%
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
%
% constraint 
%   sum(x)=72
%   % /\ increasing(x) % symmetry breaking
% ;
%
% output[
%   show(x)
% ];
