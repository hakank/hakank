% 
% Smullyan Knights and Knaves problems in MiniZinc.
%
%
% These are the problems 26 to 35 from Raymond Smullyan's wonderful book
% "What is the name of this book? - The riddle of dracula and 
% other logical puzzles".
%
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: knight = 1; % alway tells the truth
int: knave  = 2; % always lies

array[1..3] of var {knight, knave}: p; % the persons

% uncomment for problem 35
% var bool: p35_C_Says; 


%
% a knight always speaks the truth
% a knave always lies
%
% says(kind of person, what the person say)
%
predicate says(var int: kind, var bool: says) =
   (kind = knight /\ says = true )
   \/
   (kind = knave  /\ says = false )
;


solve satisfy;

constraint

    %% Problem 26: 
    %% B: A says he is a knave
    %% C: B is a knave
    %% What are B and C?
    %% Solution: A: unknown, B: knave, C: knight
    % says(p[2], says(p[1], p[1] = knave))
    % /\
    % says(p[3], p[2] = knave)

    %% Variant of problem 26, commented in problem 27, we don't need
    %% C to know that B is a knave.
    %% B: A says he is a knave 
    %% What are B?
    %% Solution: A: unknown B: knave, C: unknown
    % says(p[2], says(p[1], p[1] = knave))

    %% Problem 27
    %% B: A said that there are exactly 1 knights
    %% C: B is a knave      
    %% What are B and C
    %% Solution: B: knave, C: knight
    says(p[2], says(p[1], 1 = sum(i in 1..3) (bool2int(p[i] = 1))))
    /\
    says(p[3], p[2] = knave)

    %% Problem 28
    %% Just a and b (sets c = 1)
    %% A: at least one of us is a knave
    %% Solution: A: knight, B: knave
    % p[3] = 1 /\ % ignore C
    % says(p[1], sum(i in 1..2) (bool2int(p[i] = 2)) >= 1)

    %% Problem 29
    %% A: either A is a knave or B is a knight
    %% Ignore C
    %% Solution: A: knight, B: knight
    % p[3] = 1 /\ % ignore C
    % says(p[1], p[1] = knave \/ p[2] = knight)
    
    %% Problem 30
    %% A: either A is a knave or 2 + 2 = 5
    %% Solution: Infeasible.
    % says(p[1], p[1] = knave \/  2 + 2 = 5)
    

    %% Problem 31
    %% A: All are knaves
    %% B: Exactly one is a knight
    %% Solution: A: knave, B: knight, C: knave
    % says(p[1], sum(i in 1..3) (bool2int(p[i] = 2)) = 3)
    % /\
    % says(p[2], sum(i in 1..3) (bool2int(p[i] = 1)) = 1)

    %% Problem 32
    %% A: all are knaves
    %% B: exactly one os a knave
    % Solution: A: knave, B: knight or knave, C: knight
    % says(p[1], sum(i in 1..3) (bool2int(p[i] = 2)) = 3)
    % /\
    % says(p[2], sum(i in 1..3) (bool2int(p[i] = 2)) = 1)

    %% Problem 33
    %% A: A is knave, B is knight. Ignore C.
    %% Solutions: A: knave, B: knave
    % p[3] = 1
    % /\
    % says(p[1], p[1] = knave /\ p[2] = knight)

    %% Problem 34
    %% A: B is a knave
    %% B: A and C is of the same type
    %% Solution: A and B is not of the same type, C: knave
    % says(p[1], p[2] = knave)
    % /\
    % says(p[2], p[1] = p[3])

    %% Problem 35
    %% A: B and C are of the same type
    %% C is asked if A is the same type as B
    %% What does C say?
    %% Two Solutions: 
    %%  A: knight, B: knight, C: knave  C answer: "yes"
    %%  A: knave, B: knight, C: knave: C answer: yes
    %% C always says "yes" 
    %% (note: uncomment the p35_C_Says variable)
    % says(p[1], p[2] = p[3])
    % /\
    % says(p[3], p35_C_Says)
    % /\
    % p35_C_Says = (p[2] = p[3])
      
    %% Problem 36: Not modelled

    %% Problem 37: Not modelled    

    %% Problem 38: Not modelled
;

output [
  "1:knight (always tells the true), 2=knave (always lies)\n",
  "p: ", show(p),"\n",
  % "p35_C_Says: ", show(p35_C_Says) % just for problem 35
]
;

