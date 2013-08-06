%
% Smullyan's Portia problem in MiniZinc.
%
% From Raymond Smullyans "What is the name of this book?",
% Chapter 5 "The mystery of Portias Caskets".
%
% Problems 67a, 67b, and 68b. (The model for Problem 68a is wrong.)
%
% Constraint programming approach. 
% Cf 
%   - smullyan_knights_knaves.mzn, 
%   - smullyan_knights_knaves_normals.mzn
%   - smullyan_knights_knaves_normals_bahava.mzn


%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 


% gold, silver, lead
array[1..3] of var bool: portrait;
array[1..3] of var bool: inscriptions;

array[1..3] of string: boxes = ["Gold","Silver","Lead"];

% For problem 68a, and 68b
% array[1..3, 1..2] of var bool: inscriptions_p68;
% array[1..3] of var 0..2: p68b_sum; % just for problem 68b

predicate says(var bool: inscription, var bool: placement) =
   (inscription = true /\ placement = true) 
   \/
   (inscription = false /\ placement = false) 
;

predicate says2(var bool: inscription, var bool: placement) =
   inscription = placement
;


solve satisfy;

constraint

   % only one portrait
   sum(i in 1..3) (bool2int(portrait[i])) = 1
   /\

   %% Problem 67a
   %% Gold: The portrait is in this casket
   %% Silver: The portrait is not in this casket
   %% Lead: The portrait is not in the Gold casket
   %% Solution: The portraint is in the Silver casket
   % 
   % at most one incription is correct
   % sum(i in 1..3) (bool2int(inscriptions[i])) <= 1
   % /\ % Gold inscription
   % says2(inscriptions[1], portrait[1]) 
   % /\ % Silver inscription
   % says2(inscriptions[2], not(portrait[2]))
   % /\ % Lead inscription
   % says2(inscriptions[3], not(portrait[1]))

   %% Problem 67b
   %% Gold: The portrait is not in the Silver casket
   %% Silver: The portrait is not in this casket
   %% Lead: The portrait is in this casket
   %% Solution: The portrait is in the Gold casket
   % 
   % at least one incription is correct
   sum(i in 1..3) (bool2int(inscriptions[i])) >= 1
   /\ % at least one incription is false
   sum(i in 1..3) (bool2int(not(inscriptions[i]))) >= 1
   /\ % Gold inscription
   says2(inscriptions[1], not(portrait[2])) 
   /\ % Silver inscription
   says2(inscriptions[2], not(portrait[2]))
   /\ % Lead inscription
   says2(inscriptions[3], portrait[3])
   

   %% Problem 68a
   %% Each casket has two inscriptions, each contains no more than
   %% one false statement.
   %% Gold: 1) The portrait is not here
   %%       2) The artist of the portrait is from venice
   %% Silver: 1) The portrait is not in the Gold casket
   %%         2) The artist of the portrait is really from Florence
   %% Lead: 1) The portrait is not in here
   %%       2) The portrait is really in the Silver casket
   %% NOTE: Not a correct model!
   % says(inscriptions_p68[1,1], not(portrait[1]))
   % /\
   % says(inscriptions_p68[1,2], true)
   % /\
   % says(inscriptions_p68[2,1], not(portrait[1]))
   % /\
   % says(inscriptions_p68[2,2], false)
   % /\
   % says(inscriptions_p68[3,1], not(portrait[3]))
   % /\
   % says(inscriptions_p68[3,2], portrait[2])
   % /\ % no more than one false statement per inscription
   % forall(i in 1..3) (
   %   sum(j in 1..2) (bool2int(not(inscriptions_p68[i,j]))) <= 1
   % )


   %% Problem 68b
   %% Each casket has two inscriptions, one has two correct statements,
   %% one has two false statements and one has one true and one false
   %  statements
   %% Gold: 1) The portrait is not here
   %%       2) The portrait is in the Silver casket
   %% Silver: 1) The portrait is not in the Gold casket
   %%         2) The portrait is in the Lead casket
   %% Lead: 1) The portrait is not here
   %%       2) It is in the Gold casket
   %% Solution: The portrait is in the Lead basket.
   %% Gold has one true and one false statements, Silver has two true statement,
   %% and Lead has two false statements.
   % says(inscriptions_p68[1,1], not(portrait[1]))
   % /\
   % says(inscriptions_p68[1,2], portrait[2])
   %/\
   % says(inscriptions_p68[2,1], not(portrait[1]))
   % /\
   % says(inscriptions_p68[2,2], portrait[3])
   % /\
   % says(inscriptions_p68[3,1], not(portrait[3]))
   % /\
   % says(inscriptions_p68[3,2], portrait[1])
   % /\ % sum the number of true values
   % forall(i in 1..3) (
   %   p68b_sum[i] = sum(j in 1..2) (bool2int(inscriptions_p68[i,j]))
   % )
   % /\
   % all_different(p68b_sum)
 
;


output [
  "portrait    : ", show(portrait), "\n",
  "inscriptions: ", show(inscriptions), "\n"
%  "inscriptions_p68: ", show(inscriptions_p68), "\n" % for problem 68a and b
]
++
[
  if fix(portrait[i]) then
    "\nThe portrait is in the " ++ show(boxes[i]) ++ " casket."
  else 
    ""
  endif
  | i in 1..3  
];
