% 
% Smullyan Knight, Knave, and Normal problems in MiniZinc.
%
% This are the problem 39 to 43 from Raymond Smullyan's 
% book "What is the name of this book?".
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


include "globals.mzn"; 

int: knight = 1; % always tells the truth
int: normal = 2; % sometimes tells the truth, sometimes lies
int: knave  = 3; % always lies

array[1..3] of var {knight, knave, normal}: p;

% uncomment for problem 43
% var bool: p43_C_says_B_higher_rank_than_A;


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

solve satisfy;

constraint
    %%  Problem 39
    %% A: A is normal   
    %% B: A is normal
    %% C: C is not normal
    %% Solution: A: knave, B, normal, C, knight
    says(p[1], p[1] = normal)
    /\
    says(p[2], p[1] = normal)
    /\
    says(p[3], not(p[3] = normal))
    /\  
    all_different(p)

    %% Problem 40
    %% A: B is a knight
    %% B: A is not a knight
    %% Prove that at least one of them is telling the truth but is not a knight
    %% (Ignore C)
    %% Three solutions:
    %%  A knave, B normal
    %%  A normal, B knight
    %%  A normal, B normal
    % p[3] = 1 /\ % ignore C
    % says(p[1], p[2] = knight)
    % /\
    % says(p[2], not(p[1] = knight))


    %% Problem 41
    %% A: B is a knight
    %% B: A is a knave
    %% Prove that either one of them is telling the truth but is not a knight
    %% or one is lying but is not a knave.
    %% Three solutions:
    %%   A is knave, B is normal
    %%   B is normal, B is knave
    %%   A is normal, B is normal
    % p[3] = 1 /\ % ignore C
    % says(p[1], p[2] = knight)
    % /\
    % says(p[2], p[1] = knave)


    %% Problem 42
    %% Rank: knight >  normal > knaves
    %% Note: The coding of the types in this model is the opposite of the ranks.
    %% A: A < B
    %% B: That is not true
    %% Solution: A: normal, B: normal
    % p[3] = 1 /\ % ignore C
    % says(p[1], -p[1] < -p[2])
    % /\
    % says(p[2], not(-p[1] < -p[2]))


    %% Problem 43
    %% A: B is of higher rank than C
    %% B: C is of higher rank than A
    %% C is asked "Who has higher rank, A or B?"
    %% The book's solution: 
    %% Whether C is a knight or knave he says that B i of higher rank than A
    %% NOTE: 
    %% This model don't give the same answer as the book, 
    %% (which means that the model is wrong or not).
    %% 
    % all_different(p)
    % /\
    % says(p[1], -p[2] > -p[3])
    % /\
    % says(p[2], -p[3] > -p[1])
    % /\
    % says(p[3], p43_C_says_B_higher_rank_than_A)
    % /\
    % p43_C_says_B_higher_rank_than_A = (-p[2] > -p[1])


;

output [
  "1:knight (alway true), 2=normal (sometimes lie), 3=knave (always false), \n",
  "p: ", show(p),"\n",
  % "p43_C_says_B_higher_rank_than_A: ", show(p43_C_says_B_higher_rank_than_A), "\n", % uncomment for problem 43
]
;
