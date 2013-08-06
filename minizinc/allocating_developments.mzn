% 
% Allocating developments in MiniZinc.
% 
% Problem from
% Cramster.com (homework help....)
% http://www.cramster.com/answers-feb-12/computer-science/csp-constraint-satisfaction_2183361.aspx?rec=0
% """
% CSP (Constraint Satisfaction Problems)
% Question 1 (15 points) CSP representation: allocating developments
% You are an architect who needs to decide how to position four establishments
% in a new mall: a Japanese restaurant, a hairstylist, a clothing store,
% and a toy store. The floor plan can be represented as a 3x3 grid
% (three rows 0,1,2 and three columns 0,1,2) and you need to place
% each shop in one cell of the grid. Based on marketing research, the
% following constraint were defined on how to position the new stores,
% given the position of existing establishments. Note that establishments
% are considered close to each other of the share an edge on the grid.
%
% 1) There is a fish and chips stand in cell (2,2).
% 2) There is a health store in cell (1,0).
% 3) The hairstylist and the clothing store should not be close to the
%    fish and chips stand
% 4) The Japanese restaurant should be close to the health store
% 5) The hairstylist and the clothing store should be close to the Japanese
%    restaurant.
% 6) The hairstylist and the clothing store should not be closed to the
%    toy store.
% 
% (15 points) Represent this problem as a CSP. Be as precise as you can
% in specifying the constraint. Also do not forget some basic constraints
% that are inherent in allocating objects in space but are not listed above.
% """
%
% Note: I has not been able to satisfy all these 7 requirements.
% However, using "use_soft_constraints = 1", there are 48 solutions
% for satisfying 6 of these.
%
% Another approach is to move the health_store to another position,
% for example x[1,1], or fish_and_chips to - say - x[2,1]; 
% then it works fine (with 8 possible layouts).
% 
%
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%
include "globals.mzn"; 

int: n = 3;
int: m = 6;

int: japanese_restaurant = 1;
int: hairstylist = 2;
int: clothing_store = 3;
int: toy_store = 4;

int: fish_and_chips = 5;
int: health_store = 6;

array[0..m] of string: obj = array1d(0..m,
                             ["<empty>            ", % 0
                              "japanese restaurant", % 1
                              "hairstylist        ", % 2
                              "clothing store     ", % 3
                              "toy store          ", % 4
                              "fish_and_chips     ", % 5
                              "health_store       "  % 6 
                             ]);

array[0..n-1, 0..n-1] of var 0..m: x;

int: use_soft_constraints = 1;

% soft version of the constraints
int: num_constraints = 7;
array[1..num_constraints] of var 0..1: satis;
array[1..num_constraints] of int: prefs = [0,0,1,1,1,0,0];
array[1..num_constraints] of var 0..1: sat_prefs;
var 0..num_constraints: max_prefs = sum(sat_prefs);

predicate alldifferent_except_0_me(array[int] of var int: variables) =

   forall(i,j in index_set(variables) where i!= j) (
        (variables[i] > 0 /\ variables[j] > 0) -> variables[i] != variables[j]      
   )
;

% This is a von Neumann grid, i.e. connect just
% horizontally or vertically, but not diagonally.
predicate close(array[int,int] of var int: x,
               int: s1,
               int: s2) =
    exists(i,j in 0..n-1, a, b in {-1,0,1}
     where i+a >= 0   /\ j+b >=  0 /\
           i+a <= n-1 /\ j+b <= n-1
           /\(abs(a) + abs(b) = 1)
       ) (
        x[i,j] = s1 /\
        x[i+a, j+b] = s2
    )
;

% soft version of close
predicate closec(array[int,int] of var int: x,
               int: s1,
               int: s2,
               var 0..1: b) =
    exists(i,j in 0..n-1, a, b in {-1,0,1}
      where i+a >= 0   /\ j+b >=  0 /\
           i+a <= n-1 /\ j+b <= n-1
           /\(abs(a) + abs(b) = 1)
       ) (
        x[i,j] = s1 /\
        x[i+a, j+b] = s2
    ) <-> (b = 1)
    
;

solve satisfy;
% solve maximize max_prefs;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

%
% ensure that all objects are placed exactly once
%
constraint
   alldifferent_except_0([x[i,j] | i,j in 0..n-1])  /\

   forall(o in 1..m) (
      exists(i,j in 0..n-1) (
          x[i,j] = o
      )
   )
;

%
% Given constraints
% 
constraint
   % 1) There is a fish and chips stand in cell (2,2).
   x[2,2] = fish_and_chips /\

   % 2) There is a health store in cell (1,0).
   x[1,0] = health_store
   % x[1,1] = health_store
;



% hard constraint version
constraint
  if use_soft_constraints = 0 then
  
   % 3) The hairstylist and the clothing store should not be close to the
   %    fish and chips stand
   not(close(x, hairstylist, fish_and_chips)) /\
   not(close(x, clothing_store, fish_and_chips))  /\

   % 4) The Japanese restaurant should be close to the health store
   close(x, japanese_restaurant, health_store) /\

   % 5) The hairstylist and the clothing store should be close to the Japanese
   %    restaurant.
   close(x, hairstylist, japanese_restaurant) /\
   close(x, clothing_store, japanese_restaurant) /\

   % 6) The hairstylist and the clothing store should not be closed to the
   %    toy store.
   not(close(x, hairstylist, toy_store)) /\
   not(close(x, clothing_store, toy_store))

   /\ % take care of the soft constraint variables
   forall(c in 1..num_constraints) (
       sat_prefs[c] = bool2int(satis[c] = prefs[c]) /\
       sat_prefs[c] = 1
   )

  else 
    true
  endif
;


% soft constraint version
constraint
  if use_soft_constraints = 1 then

   % 3) The hairstylist and the clothing store should not be close to the
   %    fish and chips stand
   closec(x, hairstylist, fish_and_chips, satis[1]) /\
   closec(x, clothing_store, fish_and_chips, satis[2])  /\

   % 4) The Japanese restaurant should be close to the health store
   closec(x, japanese_restaurant, health_store, satis[3]) /\

   % 5) The hairstylist and the clothing store should be close to the Japanese
   %    restaurant.
   closec(x, hairstylist, japanese_restaurant, satis[4]) /\
   closec(x, clothing_store, japanese_restaurant, satis[5]) /\

   % 6) The hairstylist and the clothing store should not be closed to the
   %    toy store.
   closec(x, hairstylist, toy_store, satis[6]) /\
   closec(x, clothing_store, toy_store, satis[7])

   /\
   forall(c in 1..num_constraints) (
       sat_prefs[c] = bool2int(satis[c] = prefs[c])
   )
   /\
   max_prefs >= 6
  else 
    true
  endif
;


output [
  if use_soft_constraints = 1 then 
    "Using soft constraints\n" 
  else 
    "Using hard constraints\n" 
  endif
]
++
[
  if j = 0 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 0..n-1
]
++
[
  if j = 0 then "\n" else " " endif ++
    show(obj[fix(x[i,j])])
  | i,j in 0..n-1
]
++
[
  "\n" ++
  "prefs    : " ++ show(prefs)     ++ "\n" ++
  "satis    : " ++ show(satis)     ++ "\n" ++
  "sat_prefs: " ++ show(sat_prefs) ++ "\n" ++
  "max_prefs: " ++ show(max_prefs) ++ "\n"
]
++ ["\n"]
;

