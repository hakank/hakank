% 
% Are you the one problem in MiniZinc.
%
% From
% "'Are You The One?' Finding the matching through SAS Constraint Programming"
% https://blogs.sas.com/content/operations/2018/08/14/are-you-the-one/
% """
% According to Wikipedia, "Are You The One?" is a reality TV show produced by MTV,
% in which a group of men and women are secretly paired into male-female couples via
% a matchmaking algorithm. Then, while living together, the contestants try to identify
% all of these "perfect matches." If they succeed, the entire group shares a prize of up to $1 million.
% 
% In each episode, the participants get two new pieces of information about the coupling:
%  - They get to choose one couple to go to the "truth booth" and ask if that couple
%    is a perfect match or not.
%  - They all pair up in a "matching ceremony" and get to know how many couples they
%    got right (not which ones though).
% 
% This way, after each episode they have an increasing amount of information to find
% their matches. If they don't find it by episode 10, they lose.
% 
% The math
% 
% This problem can be solved through constraint programming because we're attempting to
% find all possible solutions (couples matches) that satisfy certain constraints (such
% as "Pete and Lola are NOT a couple" or "out of these given 10 couples only 2 are correct").
% The more information you get as the weeks go by, the more constrained your problem
% is and the fewer feasible solutions you have - until you are left with only one
% (hopefully at most by week 10).
% """

% Note: The SAS solution counts the number of assignments for a boy+girl. This is
% not possible in plain MiniZinc.
% Instead, see http://hakank.org/picat/are_you_the_one.pi
% for a more SAS like approach.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

/* Input number of weeks of data */
int: num_weeks = 5;

int: Anthony = 1;
int: Clinton = 2;
int: Dimitri= 3;
int: Ethan = 4;
int: Joe = 5;
int: Kareem = 6;
int: Keith = 7;
int: Malcolm = 8;
int: Michael = 9;
int: Shad = 10;
int: Tyler = 11;
set of int: boys = {Anthony,Clinton,Dimitri,Ethan,Joe,Kareem,Keith,Malcolm,Michael,Shad,Tyler};
        
int: Alexis = 12;
int: Alivia = 13;
int: Audrey = 14;
int: Diandra = 15;
int: Geles = 16;
int: Jada = 17;
int: Keyana = 18;
int: Nicole = 19;
int: Nurys = 20;
int: Uche = 21;
int: Zoe = 22;
set of int: girls = {Alexis,Alivia,Audrey,Diandra,Geles,Jada,Keyana,Nicole,Nurys,Uche,Zoe};
        
int: num_boys = card(boys);
int: num_girls = card(girls);        

/*
  Input weekly matches (from matching ceremony) up to current week.
  For example, in Season 6, as of week 5, these were the couples in each ceremony
*/
array[1..num_girls, 1..num_weeks] of girls: ceremony_pairs = array2d(1..num_girls,1..num_weeks,
[
Geles, Diandra,  Jada,  Keyana,Nicole,
Uche,  Uche,  Uche,  Uche,  Jada,
Diandra,  Nicole,Nurys, Alexis,Uche,
Jada,  Jada,  Alexis,Nicole,Geles,
Zoe,Audrey,Zoe,Zoe,Zoe,
Alivia,Alivia,Alivia,Diandra,  Alivia,
Alexis,Alexis,Diandra,  Nurys, Alexis,
Nurys, Nurys, Geles, Alivia,Diandra,
Keyana,Keyana,Audrey,Geles, Nurys,
Audrey,Geles, Keyana,Audrey,Audrey,
Nicole,Zoe,Nicole,Jada,  Keyana,
]);

% translate to [week, boy, girl]        
array[1..num_weeks,1..num_boys,1..3] of int: pairs = array3d(1..num_weeks, 1..num_boys,1..3,
[
if j = 1 then w elseif j = 2 then b else ceremony_pairs[b,w] endif 
| w in 1..num_weeks, b in 1..num_boys, j in 1..3
]);

/* Input number of correct matches (from matching ceremony) up to current week 
   For example, in Season 6, as of week 5, these were the correct guesses */
array[1..num_weeks] of int: correct_matches =  [3,1,2,3,1];
 
/* Input couples that went to truth booth and their outcome (1 if they are a match,
   0 otherwise) up to current week.
  For example, in Season 6, as of week 5, these were the truth booths
*/
array[1..num_weeks, 1..3] of int: truth_booth = array2d(1..num_weeks, 1..3,
[
Ethan,    Keyana,   0,
Anthony,  Geles,    0,
Malcolm,  Nurys,    0,
Dimitri,  Nicole,   0,
Clinton,  Uche,     0,
]);


/* Decision variables
   Assign[b,g] = 1 if boy b is assigned to girl g in the matching; 0 otherwise
*/
array[boys,girls] of var 0..1: assign;
        
solve satisfy;

        
constraint
forall(b in boys) ( 
 sum(assign[b,..]) = 1
);

constraint
forall(g in girls) ( 
  sum(assign[..,g]) = 1        
);

constraint
forall(w in 1..num_weeks) (
  sum([assign[pairs[w,b,2],pairs[w,b,3]] | b in 1..num_boys]) = correct_matches[w]
);    

constraint
forall(w in 1..num_weeks) (
  assign[truth_booth[w,1],truth_booth[w,2]] = truth_booth[w,3]
);

output [
  if g = Alexis then "\n" else " " endif ++
    show(assign[b,g])
  | b in boys, g in girls
];

