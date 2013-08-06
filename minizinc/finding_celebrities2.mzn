% 
% Finding celebrities problem (using set constraints) in MiniZinc.
%
% This version is a variant of 
%   http://www.hakank.org/minizinc/finding_celebrities.mzn
% and uses just set constraints, i.e. no conversion to incidence matrix.
%
% From Uwe Hoffmann
% "Finding celebrities at a party"
% http://www.codemanic.com/papers/celebs/celebs.pdf
% """
% Problem: Given a list of people at a party and for each person the list of
% people they know at the party, we want to find the celebrities at the party. 
% A celebrity is a person that everybody at the party knows but that 
% only knows other celebrities. At least one celebrity is present at the party.
% """
% (This paper also has an implementation in Scala.)
% 
% Note: The original of this problem is 
%   Richard Bird and Sharon Curtis: 
%   "Functional pearls: Finding celebrities: A lesson in functional programming"
%   J. Funct. Program., 16(1):13–20, 2006.
% but I (as well as Hoffmann) have not been able to access this paper.
%
% The problem from Hoffmann's paper is to find of who are the 
% celebrity/celebrities in this party graph:
%   Adam  knows {Dan,Alice,Peter,Eva},
%   Dan   knows {Adam,Alice,Peter},
%   Eva   knows {Alice,Peter},
%   Alice knows {Peter},
%   Peter knows {Alice}
% 
% Solution: the celebrities are Peter and Alice.
%
% Note: In this version we don't assume that a person knows him/herself, just
%       to be able to use the same data as finding_celebrities.mzn
%
% Note2: I blogged about this model in 
% "Finding celebrities at a party"
% http://www.hakank.org/constraint_programming_blog/2010/01/finding_celebrities_at_a_party.html
%
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n; % number of persons at the party 
% the graph of who knows who
array[1..n] of set of 1..n: party;

var set of 1..n: celebrities;
% number of celebrities
var 0..n: num_celebrities = card(celebrities);


% solve satisfy;
solve :: set_search([celebrities], first_fail, indomain_min, complete) satisfy;

constraint
  % there is at least one celebrity
  num_celebrities >= 1

  /\ % all persons know the celebrities,
     % and the celebrities only know celebrities
  forall(i in 1..n) (
     (i in celebrities -> forall(j in 1..n where j != i) (i in party[j]))
     /\
     (i in celebrities -> card(party[i]) = num_celebrities-1)
  )
;

%
% Data
%

%
% The party graph of the example above:
%
%  Adam  knows {Dan,Alice,Peter,Eva},  {2,3,4,5}
%  Dan   knows {Adam,Alice,Peter},     {1,4,5}
%  Eva   knows {Alice,Peter},          {4,5}
%  Alice knows {Peter},                {5}
%  Peter knows {Alice}                 {4}
%
% This corresponds to the matrix:
%  % 1 2 3 4 5
%    1,1,1,1,1, % 1
%    1,1,0,1,1, % 2
%    0,0,1,1,1, % 3
%    0,0,0,1,1, % 4
%    0,0,0,1,1  % 5
%
n = 5;
party = [
         {2,3,4,5}, % 1, Adam
         {1,4,5},   % 2, Dan 
         {4,5},     % 3, Eva
         {5},       % 4, Alice
         {4}        % 5, Peter
        ];


% In this example Alice (4) also knows Adam (1),
% which makes Alice a non celebrity, and since
% Peter (5) knows Alices, Peter is now also a
% non celebrity. Which means that there are no
% celebrities at this party.
% 
% n = 5;
% party = [
%          {2,3,4,5}, % 1, Adam
%          {1,4,5},   % 2, Dan 
%          {4,5},     % 3, Eva
%          {1,5},       % 4, Alice
%          {4}        % 5, Peter
%         ];


%
% Here is another example. It has the following
% cliques:
%  {1,2}
%  {4,5,6}
%  {6,7,8}
%  {3,9,10}
%
% The celebrities are {3,9,10}
%
% n = 10;
% party = [
%           {2,3,8,9,10},     % 1
%           {1,3,9,10},       % 2
%           {9,10},           % 3
%           {2,3,5,6,9,10}, % 4
%           {3,4,6,9,10},   % 5
%           {3,4,5,7,8,9,10}, % 6
%           {3,6,8,9,10},   % 7
%           {3,6,7,9,10},   % 8
%           {3,10},         % 9
%           {3,9},         % 10
% ];


%
% This is the same graph as the one above
% with the following changes:
%   - 9 don't know 3 or 10
% This party graph know consists of just 
% one celebrity: {9}
%
% n = 10;
% party = [
%           {2,3,8,9,10},     % 1
%           {1,3,9,10},       % 2
%           {9,10},           % 3
%           {2,3,5,6,9,10}, % 4
%           {3,4,6,9,10},   % 5
%           {3,4,5,7,8,9,10}, % 6
%           {3,6,8,9,10},   % 7
%           {3,6,7,9,10},   % 8
%           {},         % 9
%           {3,9},         % 10
% ];


output [
"party: " ++ show(party) ++ "\n"++
"celebrities: " ++ show(celebrities) ++ "\n"
];
