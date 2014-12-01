% 
% Ola's Kraftwerk problem in MiniZinc.
% 
% A friend of mine, Ola, and some of his friends are planning for
% buying tickets to some of the Kraftwerk 8 concerts. 
% 
% An interesting constraint in the buying procedure is that a 
% person can buy tickets for one concerts and maximum 6 tickets.
% 
% From http://www.dr.dk/DRPresse/Artikler/2014/11/10/093523.htm
% Danish:
% """
% Man kan højst købe 6 billetter til en koncert og kun 1 koncert.
% """
% (Translated: 
% "You can buy a maximum of six tickets to a concert and only one concert."
% )

% 
% The objective is then to optimize the tickets bought so they can 
% attend as many preferred concerts as possible.
%
% In the model this mean to maximize the sum of all the 
% preference values of the attending concerts (variable "z").
% 
% To simplify, this model assumes that a person can (and must)
% buy as many tickets as (s)he will attend (and vice versa).
% However, a person don't need to go to the concert (s)he buy tickets for.
%
% Input:
%   - a preference matrix where a person grade the interest of a concert
%     (larger value is better). 0 (zero) means no interest.
%   - a budget array ("attending_budget"): how many concerts can the person
%     afford to attend (or has the time etc).


%
% (One) optimal solution with the simulated preferences and budget:
%
%   z: 122
%   buy_concert: [1, 4, 2, 3, 8, 6]
%   buy_num_tickets: [5, 2, 5, 3, 4, 2]
%   Person 1 attends these concerts: [1, 2, 3, 4, 8]
%   Person 2 attends these concerts: [6, 8]
%   Person 3 attends these concerts: [1, 2, 3, 4, 8]
%   Person 4 attends these concerts: [1, 2, 8]
%   Person 5 attends these concerts: [1, 2, 3, 6]
%   Person 6 attends these concerts: [1, 2]
%
%   Person 1 buys: 5 tickets for concert 1
%   Person 2 buys: 2 tickets for concert 4
%   Person 3 buys: 5 tickets for concert 2
%   Person 4 buys: 3 tickets for concert 3
%   Person 5 buys: 4 tickets for concert 8
%   Person 6 buys: 2 tickets for concert 6
%
%   attending matrix:
%   1 1 1 1 0 0 0 1
%   0 0 0 0 0 1 0 1
%   1 1 1 1 0 0 0 1
%   1 1 0 0 0 0 0 1
%   1 1 1 0 0 1 0 0
%   1 1 0 0 0 0 0 0
%   objective matrix:
%   7 5 4 8 0 0 0 6
%   0 0 0 0 0 8 0 7
%   1 4 7 3 0 0 0 6
%   6 7 0 0 0 0 0 4
%   6 3 7 0 0 8 0 0
%   8 7 0 0 0 0 0 0
%   Who's attending each concert:
%   People attending concert 1: [1, 3, 4, 5, 6]
%   People attending concert 2: [1, 3, 4, 5, 6]
%   People attending concert 3: [1, 3, 5]
%   People attending concert 4: [1, 3]
%   People attending concert 5: []
%   People attending concert 6: [2, 5]
%   People attending concert 7: []
%   People attending concert 8: [1, 2, 3, 4]
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_people;
int: num_concerts;

% can buy max tickets per person (for one concert)
int: max_tickets_per_person;

% preferences
array[1..num_people, 1..num_concerts] of int: prefs; 

% the max number of concerts a person can (afford) to attend
% (and thus to buy tickets for)
array[1..num_people] of int: attending_budget;

% decision variables

% which concert to buy ticket for (if any) and how many
array[1..num_people] of var 0..num_concerts: buy_concert; % what concert (if any) does p buy?
array[1..num_people] of var 0..max_tickets_per_person: buy_num_tickets; % how many tickets?

% which concert to attend (if any)
% 
array[1..num_people,1..num_concerts] of var 0..1: attending;

% objective: maximize the total sum of happiness
var 0..num_people*num_concerts*num_concerts: z = sum([prefs[p,c]*attending[p,c] | p in 1..num_people, c in 1..num_concerts]); 
% var int: z = sum([prefs[p,c]*attending[p,c] | p in 1..num_people, c in 1..num_concerts]); 


% solve satisfy;
% solve maximize z;
solve :: int_search(
   buy_num_tickets ++ buy_concert
   % ++ [attending[p,c] | p in 1..num_people, c in 1..num_concerts] 
   , 
   max_regret, % first_fail, 
   indomain_max, 
   complete) 
  maximize z;
  % satisfy;

% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  alldifferent_except_0(buy_concert) 

  /\ % maximum number of tickets to buy (the budget constraint)
  forall(p in 1..num_people) (
    buy_num_tickets[p] <= attending_budget[p]
  )

  /\ % total number of bought tickets == number of attended concerts
  sum(buy_num_tickets) = sum([attending[p,c] | p in 1..num_people, c in 1..num_concerts])

  /\ % sum of tickets to this concert == people attending this concert
  forall(c in 1..num_concerts) (
    sum([attending[p,c] | p in 1..num_people]) = 
         sum([ buy_num_tickets[p]*bool2int(buy_concert[p] == c) | p in 1..num_people])
       
  )
  /\ % If a person don't buy a ticket the (s)he don't go to any concert, and vice versa.
  forall(p in 1..num_people) (
      buy_concert[p] = 0 <-> buy_num_tickets[p] = 0
  )

  % /\ 
  % forall(p in 1..num_people) (
  %   (buy_concert[p] > 0 -> buy_num_tickets[p] = sum([attending[p2,buy_concert[p]] | p2 in 1..num_people]))
  % )

  /\ % A person will attend the same number of concerts as (s)he buys tickets for
  forall(p in 1..num_people) (
     buy_num_tickets[p] = sum([attending[p,c] | c in 1..num_concerts])
  )

  /\ % a 0 preference on a concert disallow attendance on that concert
  forall(p in 1..num_people, c in 1..num_concerts) (
     if prefs[p,c] = 0 then attending[p,c] = 0 else true endif
  )

;

output [

  "z: " ++ show(z) ++ "\n" ++
  "buy_concert: " ++ show(buy_concert) ++ "\n" ++
  "buy_num_tickets: " ++ show(buy_num_tickets) ++ "\n"
]
++
[ "Person " ++ show(p) ++ " attends these concerts: " ++ show([ c | c in 1..num_concerts where fix(attending[p,c] = 1) ]) ++ "\n"

  | p in 1..num_people
]
++ ["\n"] ++
[
 "Person " ++ show(p) ++ " buys: " ++ show(buy_num_tickets[p]) ++ " tickets for concert " ++ show(buy_concert[p]) ++ "\n"

  | p in 1..num_people
]
++
["\nattending matrix:" ] 
++
[
  if c = 1 then "\n" else " " endif ++
    show(attending[p,c])
   
  | p in 1..num_people, c in 1..num_concerts
]
++
["\nobjective matrix:" ] 
++
[
  if c = 1 then "\n" else " " endif ++
    show(attending[p,c]*prefs[p,c])
  | p in 1..num_people, c in 1..num_concerts
]
++
["\nWho's attending each concert:\n"]
++
[
  "People attending concert " ++ show(c) ++ ": " ++ show([p | p in 1..num_people where fix(attending[p,c]) = 1]) ++ "\n"
  | c in 1..num_concerts
]
;

%
% data
%
num_people = 6;
num_concerts = 8;
max_tickets_per_person = 6;

% preferences: higher value is more preferred
prefs = array2d(1..num_people, 1..num_concerts,
  [
   7,5,4,8,3,2,1,6, % Ola
   0,0,0,5,0,8,6,7, % Paulina (not interested in some concerts)
   1,4,7,3,5,2,8,6, % Olof
   6,7,3,1,8,2,5,4, % Jens
   6,3,7,5,2,8,4,1, % Fredrik
   8,7,4,2,1,6,5,3  % Jens' friend 

   % 8,7,6,5,4,3,2,1,
   % 8,7,6,5,4,3,2,1,
   % 8,7,6,5,4,3,2,1,
   % 8,7,6,5,4,3,2,1,
   % 8,7,6,5,4,3,2,1,
   % 8,7,6,5,4,3,2,1,

  ]);

% budget for buying/attending
attending_budget = [8,2,5,3,4,2];


