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

% decision variables
array[1..n] of var set of 1..n: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  
  forall(i in 1..n) (
    % speak one's native language
    i in x[i] 
    /\
    % all translators speak three languages
    card(x[i]) = 3
  )

  % 1. The Englishman speaks German.
  /\ German in x[English] 

  % 2. The Spaniard speaks French.
  /\ French in x[Spanish]

  % 3. The German does not speak Spanish.
  /\ not(Spanish in x[German])

  % 4. The Frenchman does not speak German.
  /\ not(German in x[French])

  % 5. Spaniard and Englishman can talk with each other in German.
  /\ German in x[Spanish]
  /\ German in x[English]

  % 6. Englishman and Frenchman can talk with each other in two languages.
  /\ card(x[English] intersect x[French]) = 2

  % 7. Four translators speak Russian.
  /\ sum([bool2int(Russian in x[i]) | i in 1..n]) = 4

  % 8. Exactly two translators speak French.
  /\ sum([bool2int(French in x[i]) | i in 1..n]) = 2

  % 9. Only one translator who speaks Spanish speaks also Russian.
  /\ sum([bool2int(Spanish in x[i] /\ Russian in x[i]) | i in 1..n]) = 1

  % 10. Russian and Englishman have no common languages except their native languages.
  /\ sum([bool2int(i in x[English] /\ i in x[Russian]) | i in {Spanish,French,German}]) = 0

;

output 
[ "x: " ++ show(x) ++ "\n" ++ "Translators:\n" ]
++
[
  "The " ++ lang[i] ++ " translator speaks: " ++ show(join(" ", [lang[ll] | ll in fix(x[i])])) ++ "\n"
  | i in 1..n
]
++
["\nLanguages:"]
++
[
  if j = 1 then "\nLanguage " ++ show(lang[i]) ++ " is spoken by the " else "" endif ++ 
  if i in fix(x[j]) then show(lang[j]) ++ " " else "" endif
  | i in 1..n, j in 1..n
];

