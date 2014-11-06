% 
% Five translator problem in MiniZinc.
% 
% http://stackoverflow.com/questions/26260407/prolog-logic-puzzle-constraint-programming
% """
% Five translators are working in an international organization:
% Spaniard, Englishman, Frenchman, German and Russian.
% Each of them speaks its native language and also two
% languages from the native languages of other translators. Find
% the languages speaked by each translator if it is known that
% 1. The Englishman speaks German.
% 2. The Spaniard speaks French.
% 3. The German does not speak Spanish.
% 4. The Frenchman does not speak German.
% 5. Spaniard and Englishman can talk with each other in German.
% 6. Englishman and Frenchman can talk with each other in two languages.
% 7. Four translators speak Russian.
% 8. Exactly two translators speak French.
% 9. Only one translator who speaks Spanish speaks also Russian.
% 10. Russian and Englishman have no common languages except
% their native languages.
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;

int: Spanish = 1;
int: English = 2;
int: French = 3;
int: German = 4;
int: Russian = 5;

array[1..n] of string: lang = ["Spanish","English","French","German","Russian"];
array[1..n] of string: lang1 = ["S","E","F","G","R"];

% decision variables
array[1..n,1..n] of var 0..1: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  
  forall(i in 1..n) (
    % one speak one's native language
    x[i,i] = 1
    /\
    % all translators speak three languages
    sum([x[i,j] | j in 1..n]) = 3
  )

  % 1. The Englishman speaks German.
  /\ x[English,German] = 1

  % 2. The Spaniard speaks French.
  /\ x[Spanish,French] = 1

  % 3. The German does not speak Spanish.
  /\ x[German,Spanish] = 0

  % 4. The Frenchman does not speak German.
  /\ x[French,German] = 0

  % 5. Spaniard and Englishman can talk with each other in German.
  /\ x[Spanish,German] = 1
  /\ x[English,German] = 1

  % 6. Englishman and Frenchman can talk with each other in two languages.
  /\ sum([bool2int(x[English,j] = x[French,j] /\ x[French,j] = 1) | j in 1..n]) = 2

  % 7. Four translators speak Russian.
  /\ sum([x[i,Russian] | i in 1..n]) = 4

  % 8. Exactly two translators speak French.
  /\ sum([x[i,French] | i in 1..n]) = 2

  % 9. Only one translator who speaks Spanish speaks also Russian.
  /\ sum([bool2int(x[i,Spanish] = 1 /\ x[i,Russian] = 1) | i in 1..n]) = 1

  % 10. Russian and Englishman have no common languages except their native languages.
  /\ sum([bool2int(x[English,j] = x[Russian,j] /\ x[Russian,j] = 1 ) | j in {Spanish,French,German}]) = 0

;

output 
[
  "S E F G R"
]
++
[
 if j = 1 then "\n" else " " endif ++
   show(x[i,j])
 ++ 
 if j = n then "  " ++ show(lang1[i]) else "" endif
 | i,j in 1..n
]
;

