% 
% Smullyan Knight, Knave, and Normal, Island of Bahava problems in MiniZinc.
%
% This are the problem 44, 45, 46 from Raymond Smullyan's 
% book "What is the name of this book?".
%
% The rules for The Island of Bahava:
% A knight must marry a knave
% A knave must marry a knight
% A normal must marry a normal

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


include "globals.mzn"; 

int: knight = 1; % always tells the truth
int: normal = 2; % sometimes tells the truth, sometimes lies
int: knave  = 3; % always lies

array[1..2] of var {knight, knave, normal}: mr_p;
array[1..2] of var {knight, knave, normal}: mrs_p;

%
% a knight always speaks the truth
% a knave always lies
% a normal sometimes lies
%
% says(kind of person, what the person say)
%
predicate says(var int: kind, var bool: says) =
   (kind = knight /\ says = true )
   \/
   (kind = knave  /\ says = false )
   \/ % if not a knight or a knave
   (kind = normal)
;

predicate marriage_rules(array[int] of var int: mr_p, array[int] of var int: mrs_p) =
  forall(i in 1..2) (
     (mr_p[i] = normal /\ mrs_p[i] = normal) 
     \/
     (mr_p[i] = knight /\ mrs_p[i] = knave)
     \/
     (mr_p[i] = knave /\ mrs_p[i] = knight)
 
  )  
;


solve satisfy;

constraint

  marriage_rules(mr_p, mrs_p)
  /\

  %% Problem 44
  %% Mr A: My wife is not normal
  %% Mrs A: My husband is not normal
  %% Solution: Both are normal
  % ignore mr and mrs B
  mr_p[2] = knight /\ mrs_p[2] = knave
  /\
  says(mr_p[1], not(mrs_p[1] = normal))
  /\
  says(mrs_p[1], not(mr_p[1] = normal))


  %% Problem 45
  %% Mr A: My wife is normal
  %% Mrs A: My husband is normal
  %% Solution: Both are normal
  % % ignore Mr and Mrs B
  % mr_p[2] = knight /\ mrs_p[2] = knave
  % /\
  % says(mr_p[1], mrs_p[1] = normal)
  % /\
  % says(mrs_p[1], mr_p[1] = normal)

  %% Problem 46
  %% Mr A: Mr B is a knight
  %% Mrs A: My husband is right. Mr B is a knight
  %% Mrs B: That's right. My husband is indeed a knight.
  %% Solution: All are normal
  % says(mr_p[1], mr_p[2] = knight)
  % /\
  % says(mrs_p[1], mr_p[2] = knight)
  % /\
  % says(mrs_p[2], mr_p[2] = knight)

;

output [
  "1:knight (alway true), 2=normal (sometimes lie), 3=knave (always false), \n",
  "mr_p: ", show(mr_p),"\n",
  "mrs_p: ", show(mrs_p),"\n",

]
;
