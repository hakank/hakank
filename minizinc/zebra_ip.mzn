% 
% Zebra problem, integer programming model in MiniZinc.
% 
% This is an integer programming model of the Zebra problem.
% It was remodelled from the GLPK example zebra.mod
% 
% """ 
% The Zebra Puzzle is a well-known logic puzzle.
% 
%   It is often called "Einstein's Puzzle" or "Einstein's Riddle"
%   because it is said to have been invented by Albert Einstein as a boy,
%   with the common claim that Einstein said "only 2 percent of the
%   world's population can solve". It is also sometimes attributed to
%   Lewis Carroll. However, there is no known evidence for Einstein's or
%   Carroll's authorship.
% 
%   There are several versions of this puzzle. The version below is
%   quoted from the first known publication in Life International
%   magazine on December 17, 1962.
% 
%    1. There are five houses.
%    2. The Englishman lives in the red house.
%    3. The Spaniard owns the dog.
%    4. Coffee is drunk in the green house.
%    5. The Ukrainian drinks tea.
%    6. The green house is immediately to the right of the ivory house.
%    7. The Old Gold smoker owns snails.
%    8. Kools are smoked in the yellow house.
%    9. Milk is drunk in the middle house.
%   10. The Norwegian lives in the first house.
%   11. The man who smokes Chesterfields lives in the house next to the
%       man with the fox.
%   12. Kools are smoked in the house next to the house where the horse
%       is kept.
%   13. The Lucky Strike smoker drinks orange juice.
%   14. The Japanese smokes Parliaments.
%   15. The Norwegian lives next to the blue house.
% 
%   Now, who drinks water? Who owns the zebra?
% 
%   In the interest of clarity, it must be added that each of the five
%   houses is painted a different color, and their inhabitants are of
%   different national extractions, own different pets, drink different
%   beverages and smoke different brands of American cigarettes. One
%   other thing: In statement 6, right means your right.
% 
%   (From Wikipedia, the free encyclopedia.)
% """


% Answer:        
%                Houses ->
% colors       : [5, 1, 4, 3, 2]
% nationalities: [3, 5, 1, 4, 2]
% drinks       : [5, 4, 2, 3, 1]
% smokes       : [2, 1, 4, 3, 5]
% pets         : [2, 3, 4, 1, 5]


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

%
% converts a binary matrix to a number array
% 
predicate binmatrix2num(array[int,int] of var int: x, array[int] of var int: nums) =

  forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
     nums[i] = j <-> x[i,j] = 1
  )
;


set of int: HOUSE = 1..5;

int: blue   = 1;
int: green  = 2;
int: ivory  = 3;
int: red    = 4;
int: yellow = 5;
set of int: COLOR = 1..5; %  { blue, green, ivory, red, yellow };
array[1..5] of var 1..5: colors;

int: Englishman = 1;
int: Japanese   = 2;
int: Norwegian  = 3;
int: Spaniard   = 4;
int: Ukranian   = 5;
set of int: NATIONALITY = 1..5; % { Englishman, Japanese, Norwegian, Spaniard,      Ukranian };
array[1..5] of var 1..5: nationalities;

int: coffee       = 1;
int: milk         = 2;
int: orange_juice = 3;
int: tea          = 4;
int: water        = 5;
set of int: DRINK = 1..5; % { coffee, milk, orange_juice, tea, water };
array[1..5] of var 1..5: drinks;

int: Chesterfield = 1;
int: Kools        = 2;
int: Lucky_Strike = 3;
int: Old_Gold     = 4;
int: Parliament   = 5;
set of int: SMOKE = 1..5; % { Chesterfield, Kools, Lucky_Strike, Old_Gold, Parliament };
array[1..5] of var 1..5: smokes;

int: dog    = 1;
int: fox    = 2;
int: horse  = 3;
int: snails = 4;
int: zebra  = 5;
set of int: PET = 1..5; % { dog, fox, horse, snails, zebra };
array[1..5] of var 1..5: pets;

%
% Setting up the array and all different constraints.
%

% color
array[HOUSE, COLOR] of var 0..1: color;
constraint
 forall(h in HOUSE) (sum(c in COLOR) (color[h,c]) = 1)
 /\
 forall(c in COLOR) (sum(h in HOUSE) (color[h,c]) = 1)
;

% nationality
array[HOUSE, NATIONALITY] of var 0..1: nationality;
constraint
  forall(h in HOUSE) (sum(n in NATIONALITY) (nationality[h,n]) = 1)
  /\
  forall(n in NATIONALITY) (sum(h in HOUSE) (nationality[h,n]) = 1)
;

% drink
array[HOUSE, DRINK] of var 0..1: drink;
constraint 
  forall(h in HOUSE) (sum(d in DRINK) (drink[h,d]) = 1)
  /\
  forall(d in DRINK) (sum(h in HOUSE) (drink[h,d]) = 1)
;

% smoke
array[HOUSE, SMOKE] of var 0..1: smoke;
constraint
  forall(h in HOUSE) (sum(s in SMOKE) (smoke[h,s]) = 1)
  /\
  forall(s in SMOKE) (sum(h in HOUSE) (smoke[h,s]) = 1)
;

% pet
array[HOUSE, PET] of var 0..1: pet;
constraint
  forall(h in HOUSE) (sum(p in PET) (pet[h,p]) = 1)
  /\
  forall(p in PET) (sum(h in HOUSE) (pet[h,p]) = 1)
;


%
% The specific constraints
%
constraint

   % the Englishman lives in the red house 
   forall(h in HOUSE) ( nationality[h,Englishman] = color[h,red])
   /\  

   % the Spaniard owns the dog 
   forall(h in HOUSE) ( nationality[h,Spaniard] = pet[h,dog])
   /\

   % coffee is drunk in the green house 
   forall(h in HOUSE) ( drink[h,coffee] = color[h,green])

   /\
   % the Ukrainian drinks tea 
   forall(h in HOUSE) ( nationality[h,Ukranian] = drink[h,tea])

   /\
   % the green house is immediately to the right of the ivory house 
   forall(h in HOUSE) (
     if h = 1 then
       color[h,green] = 0
     else 
       color[h,green] = color[h-1,ivory] 
     endif      
   )

   /\
   % the Old Gold smoker owns snails 
   forall(h in HOUSE) ( smoke[h,Old_Gold] = pet[h,snails])

   /\
   % Kools are smoked in the yellow house 
   forall(h in HOUSE) ( smoke[h,Kools] = color[h,yellow])

   /\
   % milk is drunk in the middle house 
   drink[3,milk] = 1

   /\
   % the Norwegian lives in the first house 
   nationality[1,Norwegian] = 1
   /\

   % the man who smokes Chesterfields lives in the house next to the man
   %   with the fox 
   forall(h in HOUSE) (
      (1 - smoke[h,Chesterfield]) +
      (if h = 1 then 0 else pet[h-1,fox] endif) +
      (if h = 5 then 0 else pet[h+1,fox] endif) >= 1
   )
   /\
   % Kools are smoked in the house next to the house where the horse is
   % kept 
   forall(h in HOUSE) (
      (1 - smoke[h,Kools]) +
      (if h = 1 then 0 else pet[h-1,horse] endif) +
      (if h = 5 then 0 else pet[h+1,horse] endif) >= 1
   )

   /\
   % the Lucky Strike smoker drinks orange juice 
   forall(h in HOUSE) ( smoke[h,Lucky_Strike] = drink[h,orange_juice])

   /\
   % the Japanese smokes Parliaments 
   forall(h in HOUSE) ( nationality[h,Japanese] = smoke[h,Parliament])
   /\

   % the Norwegian lives next to the blue house 
   forall(h in HOUSE) (
      (1 - nationality[h,Norwegian]) +
      (if h = 1 then 0 else color[h-1,blue] endif) +
      (if h = 5 then 0 else color[h+1,blue] endif) >= 1
   )

   % for displaying the result
   /\
   binmatrix2num(color, colors)
   /\
   binmatrix2num(nationality, nationalities)
   /\
   binmatrix2num(drink, drinks)
   /\
   binmatrix2num(smoke, smokes)
   /\
   binmatrix2num(pet, pets)


;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;


output 
[ 
  "colors       : ", show(colors), "\n",
  "nationalities: ", show(nationalities), "\n",
  "drinks       : ", show(drinks), "\n",
  "smokes       : ", show(smokes), "\n",
  "pets         : ", show(pets), "\n",
];