% 
% Recreational mathematics in MiniZinc. 
%
% Problem 1.3 from 
% Averbach & Chein "Problem Solving Through Recreational Mathematics", page 2.
% """
% Armand Alloway, Basil Bennington, Col. Carton Cunningham, Durwood Dunstan, and 
% Everitt Elmsby, Esq are the five senior members of the Devonshire Polo Club. 
% Each owns a pony that is named of the wife of one of the others.
% 
% - Mr Alloway's pony is named Geogette; 
% - Col Cunningham owns Jasmine
% - Mr Elmsby owns Inez
% - Francine, owned by Mr Dunstan is named after Alloways wife
% - Georgettes husband owns the pony that is named after Mr Bennington's wife
% - Helene Cunningham is the only wife who knows how to ride a horse.
% 
% Who is Jasmine's husband? Who owns Helene?
% """


% The solution from the book:
% 
%  Men          Pony         Wife
% -------------------------------------
% Alloway       Georgette    Francine
% Bennington    Helene       Inez
% Cunningham    Jasmine      Helene
% Dunstan       Francine     Jasmine  
% Elmsby        Inez         Georgette
% 
% I.e.
%   pony = [2, 3, 5, 1, 4]
%   wife = [1, 4, 3, 5, 2]
%

% Which is the same as the solution in this model:
% pony = [2, 3, 5, 1, 4]
% wife = [1, 4, 3, 5, 2]
% X = 5


% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

% the men
int: Alloway = 1;
int: Bennington = 2;
int: Cunningham = 3;
int: Dunstan = 4;
int: Elmsby = 5;

% the name of the wifes, and the names of the ponies
int: Francine = 1;
int: Georgette = 2;
int: Helene = 3;
int: Inez = 4;
int: Jasmine = 5;



array[1..5] of 1..5: men = [Alloway,Bennington,Cunningham,Dunstan,Elmsby];
array[1..5] of var 1..5: wife;
array[1..5] of var 1..5: pony;

var 1..5: X;

solve satisfy;

constraint
  all_different(wife) 
  /\
  all_different(pony)
  /\ % wife and pony don't have the same name
  forall(i in 1..5) (
   pony[i] != wife[i]
  )

  /\ % Mr Alloway's pony is named Geogette; 
  pony[Alloway] = Georgette
  /\ 
  wife[Alloway] != Georgette

  /\ % Col Cunningham owns Jasmine
  pony[Cunningham] = Jasmine
  /\
  wife[Cunningham] != Jasmine
 
  /\ % Mr Elmsby owns Inez
  pony[Elmsby] = Inez
  /\
  wife[Elmsby] != Inez

  /\ % Francine, owned by Mr Dunstan is named after Alloways wife
  pony[Dunstan] = Francine
  /\
  wife[Alloway] = Francine

  /\ % Georgettes husband owns the pony that is named after Mr Bennington's wife
     % "There is an X such that X is is Georgettes husband and X owns a pony
     % with the same name as Bennington's wife."
  (wife[X] = Georgette /\ pony[X] = wife[Bennington])

  /\ % Helene Cunningham is the only wife who knows how to ride a horse.
  wife[Cunningham] = Helene
;


output
[
  "wife: " ++ show(wife) ++ "\n" ++
  "pony: " ++ show(pony) ++ "\n" ++
  "X: " ++ show(X) ++ "\n" 
];


