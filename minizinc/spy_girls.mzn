% 
% Spy girls puzzle in MiniZinc.
% 
% From
% http://cs.calstatela.edu/wiki/index.php/Courses/CS_460/Fall_2006/Nadeem,_Hassan/Spy_Girls
% """
%                                        SPY GIRLS
% The Spy Girls Agency, the all-female spy agency, consists of Jane and 
% four other agents, one of whom is Agent Brookes. Each agent is ranked 
% by her secret agent number, from Agent 1 (highest) to Agent 5 (lowest). 
% From this information and the following clues, determine each agent's rank, 
% first name, and last name.
% 
%  1. Sylvia is ranked higher than Agent Adams, but is not the highest.
%  2. Hillary's rank is higher than Agent Darling, but not as high as 
%     Agent Brookes.
%  3. Amanda is ranked higher than either Kate or Hillary, but is not the 
%     highest.
%  4. Agent Adams is ranked higher than Agent Miller, but is ranked lower 
%     than Agent Cooper.
%  5. Agent Miller is not ranked the lowest.
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Answer:
%
% first_names = [1, 2, 4, 5, 3]
% last_names =  [2, 3, 5, 1, 4]
%
%
% Rank  Name
% 1     Kate Cooper
% 2     Jane Brookes
% 3     Amanda Adams
% 4     Hillary Miller
% 5     Kate Darling

include "globals.mzn"; 

int: n = 5;
set of int: r = 1..n;
var r: jane;
var r: sylvia;
var r: hillary;
var r: kate;
var r: amanda;

array[r] of var r: first_names = [jane, sylvia, hillary, kate, amanda];

var r: brookes;
var r: adams;
var r: darling;
var r: cooper;
var r: miller;
array[r] of var r: last_names = [brookes, adams, darling, cooper, miller];

solve satisfy;

constraint

   all_different(first_names)
   /\
   all_different(last_names)

   % 1 is highest rank, 5 is lowest

   % The Spy Girls Agency, the all-female spy agency, consists of Jane and 
   % four other agents, one of whom is Agent Brookes. 
   /\
   jane != brookes
 
   %  1. Sylvia is ranked higher than Agent Adams, but is not the highest.
   /\
   sylvia < adams
   /\ 
   sylvia > 1

   %  2. Hillary's rank is higher than Agent Darling, but not as high as 
   %     Agent Brookes.
   /\
   hillary < darling
   /\
   brookes < hillary


   %  3. Amanda is ranked higher than either Kate or Hillary, but is not the 
   %     highest.
   /\
   amanda < kate
   /\
   amanda < hillary
   /\
   amanda > 1


   %  4. Agent Adams is ranked higher than Agent Miller, but is ranked lower 
   %     than Agent Cooper. 
   /\
   adams < miller
   /\
   adams > cooper

   %  5. Agent Miller is not ranked the lowest.
   /\
   miller < 5

;


output [
  "first_names: ", show(first_names), "\n",
  "last_names : ", show(last_names), "\n",

];