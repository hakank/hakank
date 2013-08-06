% 
% Unique set representing puzzle in MiniZinc.
% 
% From Stack Overflow
% "Choosing unique set representing number puzzle"
% http://stackoverflow.com/questions/6689147/choosing-unique-set-representing-number-puzzle
% """
% Following is the input set given.
% 
% 1009 2000
% 1009 2001
% 1002 2002
% 1003 2002 
%
% Each line represents one group, Number represents ID of the member in the group. 
% Problem is to choose minimum number of people which re-presents complete given set. 
% Only one member should be choose from each Group. 2-tuple members will not repeated. But 
% members can be part of more than one group.
% 
% So in this example answer be 1009 and 2002 which represents the sets. 1009 is chosen 
% because it is representing two team and same is the case for 2002.
% 
% I am looking for what algorithm can be used to solve this problem.
% 
% Another example:
% 
% 1009 2000
% 1009 2001
% 1002 2002
% 1003 2002 
% 1004 2003
% 
% Answer can be { 1009 , 2002, 1004} or { 1009, 2002, 2003}
% """

% Note: This problem seems to be inspired by
% Spotify Job puzzle: Bilateral Projects
% http://www.spotify.com/us/jobs/tech/bilateral-projects/


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% int: n = 4; 
% array[1..n, 1..2] of int: input = array2d(1..n, 1..2, [1009, 2000,
%                                                        1009, 2001,
%                                                        1002, 2002,
%                                                        1003, 2002]);

int: n = 5;
array[1..n, 1..2] of int: input = array2d(1..n, 1..2, [1009, 2000,
                                                       1009, 2001,
                                                       1002, 2002,
                                                       1003, 2002,
                                                       1004, 2003]);

% example from a comment (n.m.)
% (unsolvable)
% int: n = 3;
% array[1..n, 1..2] of int: input = array2d(1..n,  1..2, [1,2,
%                                                         2,3,
%                                                         3,1
%                                                         ]);

% the ids (for the domains in s)
set of int: ids = { input[i,j] | i in 1..n, j in 1..2};

% decision variables
var set of ids: s;



% solve satisfy;
solve minimize(card(s));
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

% Testing first example (unique solution)
% constraint card(s) = 2;

% Testing second example (2 solutions)
% constraint card(s) = 3;

constraint
   forall(i in 1..n) (
      % "Only one member should be choose from each Group"
      % -> xor
      input[i,1] in s xor input[i,2] in s
   )
;

output [
  "ids: " ++ show(ids) ++ "\n" ++
  "s  : " ++ show(s) ++ "\n" ++ 
  "card(s): " ++ show(card(s)) ++ "\n"
]
 ++ ["\n"]
;

