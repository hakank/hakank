% 
% Timeslot problem in MiniZinc.
% 
% From Stack Overflow
% What category of combinatorial problems appear on the logic games section of the LSAT?
% http://stackoverflow.com/questions/2707619/what-category-of-combinatorial-problems-appear-on-the-logic-games-section-of-the
% """
% EDIT: See 
% http://stackoverflow.com/questions/318888/solving-who-owns-the-zebra-programmatically 
% for a similar class of problem
% 
% There's a category of logic problem on the LSAT that goes like this:
% 
%     Seven consecutive time slots for a broadcast, numbered in chronological order 
%     I through 7, will be filled by six song tapes-G, H, L, O, P, S-and exactly 
%     one news tape. Each tape is to be assigned to a different time slot, and 
%     no tape is longer than any other tape. The broadcast is subject to the 
%     following restrictions:
%       L must be played immediately before O.
%       The news tape must be played at some time after L.
%       There must be exactly two time slots between G and P, regardless of 
%          whether G comes before P or whether G comes after P.
%
% I'm interested in generating a list of permutations that satisfy the conditions 
% as a way of studying for the test and as a programming challenge. However, I'm 
% not sure what class of permutation problem this is. I've generalized the type 
% problem as follows:
% 
% Given an n-length array A:
% 
%     How many ways can a set of n unique items be arranged within A? 
%           Eg. How many ways are there to rearrange ABCDEFG?
%     If the length of the set of unique items is less than the length of A, 
%           how many ways can the set be arranged within A if items in the set 
%           may occur more than once? Eg. ABCDEF => AABCDEF; ABBCDEF, etc.
%     How many ways can a set of unique items be arranged within A if the 
%     items of the set are subject to "blocking conditions"?
%
% My thought is to encode the restrictions and then use something like Python's 
% itertools to generate the permutations. Thoughts and suggestions are welcome.
% """

% Note: There are 60 solutions to this specific problem.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 7;

% The songs and News
var 1..n: G;
var 1..n: H;
var 1..n: L;
var 1..n: O;
var 1..n: P;
var 1..n: S;
var 1..n: News;

% For output
array[1..n] of string: str = ["G","H","L","O","P","S","News"];

% decision variables
% Which timeslot would each song/News go?
array[1..n] of var 1..n: x = [G,H,L,O,P,S,News];

% The timeline
array[1..n] of var 1..n: timeline;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   % L must be played immediately before O.
   (O-L = 1)

   % The news tape must be played at some time after L.
   /\
   (News > L)

   % There must be exactly two time slots between G and P, regardless of    
   % whether G comes before P or whether G comes after P.
   /\ 
   abs(G-P) = 3
;

constraint
   alldifferent(x)
   /\ % convert to timeline
   inverse(x, timeline)
;


output [
   "x: " ++ show(x) ++ "\n" ++
   "timeline: "
]
++
[
  show(str[fix(timeline[i])]) ++ " "
  | i in 1..n
]
++ ["\n"]
;

