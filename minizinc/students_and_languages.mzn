% 
% Students and Languages problem in MiniZinc.
% 
% This is the Students and Languages problem from section 3.7 of
% Antoni Niederlinski: 
% "A Quick and Gentle Guide to Constraint Logic Programming via ECLiPSe",
% page 122ff.

% Problem formulation (page 122):
% """
% Five students of five nationalities spend their vacation on the Masurian
% Lakes. Its a Pole, a Hungarian, a Finn, a Swede and a German. Determine
% who speaks what language if:
% 1. Each student is fluent in one o more foreign languages, but only in those
%    that are native for some of the remaining students.
% 2. There is no single language spoken by all of them.
% 3. Each student may speak with any other student using some language.
% 4. The common languages include native languages of all students.
% 5. On average each student speaks two foreign languages.
% 6. The Pole and the Hungarian speak three foreign languages.
% 7. While the Swede has been swimming, the remaining four students could
%    speak a common language.
% 8. A common language could also be spoken while the Swede returned, but
%    the Finn went rowing.
% 9. In order to speak Swedish, two student had to leave the group.
% 10. Polish and Finnish is spoken (as foreign language) by only two students.
% 11. The Pole and Finn may communicate using two languages, none of them
%     being German.
% 12. The Hungarian and the Swede have only one common language.
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: N = 5;

int: Polish = 1; % Polish language or student
int: Hungarian = 2;
int: Finnish = 3;
int: Swedish = 4;
int: German = 5;


% decision variables
array[1..N,1..N] of var 0..1: Students;


solve satisfy;
% solve :: int_search([Students[S,L] | S in 1..N, L in 1..N], first_fail, indomain_min, complete) satisfy;

constraint
  % Five students of five nationalities spend their vacation on the Masurian
  % Lakes. Its a Pole, a Hungarian, a Finn, a Swede and a German. Determine
  % who speaks what language if:

  % Assumption: A student is fluent is his/her own language
  forall(I in 1..N) (Students[I,I] = 1) /\

  % 1. Each student is fluent in one or more foreign languages, but only in those
  %    that are native for some of the remaining students.
  forall(Student in 1..N) ( 
    sum(L in 1..N) (Students[Student,L]) >= 2
  ) 
  /\

  % 2. There is no single language spoken by all of them.
  forall(Language in 1..N) (
    sum(S in 1..N) (Students[S, Language]) < N
  ) 
  /\

  % 3. Each student may speak with any other student using some language.
  forall(Student1 in 1..N, Student2 in 1..N where Student1 < Student2) (
     exists(L in 1..N) (
         Students[Student1,L] + Students[Student2,L] = 2
     )
  ) 
  /\

  % 4. The common languages include native languages of all students.  
  % forall(Language in 1..N) (
  %   sum(S in 1..N) (Students[S,Language]) >= 2
  % )
  % /\

  % 5. On average each student speaks two foreign languages.
  % sum(S in 1..N, L in 1..N) (Students[S,L] ) div 5 = 3 /\
  % sum(S in 1..N, L in 1..N where S != L) (Students[S,L]) div 4 >= 2 /\
  % sum(S in 1..N, L in 1..N where S != L) (Students[S,L]) div 4 <= 3 /\
  sum(S in 1..N, L in 1..N where S != L) (Students[S,L]) = 10 /\

  % 6. The Pole and the Hungarian speak three foreign languages.
  sum(L in 1..N where L != Polish) (Students[Polish,L]) = 3 /\
  sum(L in 1..N where L != Hungarian) (Students[Hungarian,L]) = 3 /\


  % 7. While the Swede has been swimming, the remaining four students could
  %    speak a common language.
  exists(L in 1..N) (
     sum(S in [Polish,Hungarian,Finnish,German]) (Students[S,L]) = 4
  )
  /\
 
  % 8. A common language could also be spoken while the Swede returned, but
  %    the Finn went rowing.
  exists(L in 1..N) (
     sum(S in [Polish,Hungarian,Swedish,German]) (Students[S,L]) = 4
  ) 
  /\

  % 9. In order to speak Swedish, two student had to leave the group.
  sum(S in 1..N) (Students[S,Swedish]) = 3 /\

  % 10. Polish and Finnish is spoken (as foreign language) by only two students.
  sum(S in 1..N where S != Polish) (Students[S,Polish]) +
      sum(S in 1..N where S != Finnish) (Students[S,Finnish]) = 2 /\

  % 11. The Pole and Finn may communicate using two languages, none of them
  %     being German.
  sum(L in 1..N) (bool2int(Students[Polish,L] = Students[Finnish,L])) = 2 /\
  Students[Polish,German] + Students[Finnish,German] < 2 /\

  % 12. The Hungarian and the Swede have only one common language.
  sum(L in 1..N) (bool2int(Students[Hungarian,L] = Students[Swedish,L])) = 1

;

array[1..N] of string: ss = ["Polish",
                             "Hungarian",
                             "Finnish",
                             "Swedish",
                             "German"];

output [
  "The " ++ ss[S] ++ " student speaks: " ++ show(join(", ", [ss[L] | L in 1..N where fix(Students[S,L]) = 1])) ++ "\n"
  | S in 1..N
];

