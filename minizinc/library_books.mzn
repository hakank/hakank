% 
% Library Books problem (Logic4Fun) in MiniZinc.
% 
% http://l4f.cecs.anu.edu.au/puzzles/beginner/library-books
% """
% Recently the library send out nasty letters to students who had overdue books 
% on loan. Three students, Arthur, Belinda and Clarissa, had exactly 25 overdue 
% books between them, though I’m glad to say no one individual exceeded last 
% year’s record number of 14.
% 
% They are (not necessarily respectively) majoring in English, French and German.
% 
% The student of French has three more overdue books than Arthur, and Clarissa 
% has two more overdue books than the person majoring in English.
% 
% Who is studying what, and how many books do they need to return to the library?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 3;

int: Arthur = 1;
int: Belinda = 2;
int: Clarissa = 3;
array[1..n] of string: people_s = ["Arthur","Belinda","Clarissa"];

int: English = 1;
int: French = 2;
int: German = 3;

array[1..n] of string: major_s = ["English","French","German"];

% decision variables
array[1..n] of var 1..n: major;
array[1..n] of var 1..n: major_inv;

% "... though I’m glad to say no one individual exceeded last year’s record number of 14."
array[1..n] of var 1..14: returned;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(major) 

  % "Three students, Arthur, Belinda and Clarissa, had exactly 25 overdue 
  %  books between them."
  /\ sum(returned) = 25

  % "The student of French has three more overdue books than Arthur,"
  /\ returned[major[French]] = returned[Arthur] + 3
  /\ major[French] != Arthur

  % "and Clarissa has two more overdue books than the person majoring in English."
  /\ returned[Clarissa] = returned[major[English]] + 2
  /\ major[English] != Clarissa

  % for easier presentation
  /\ inverse(major,major_inv)
;

output [
  "major   : ", show(major), "\n",
  "major_inv: ", show(major_inv), "\n",
  "returned: ", show(returned), "\n"
]
++
[
  show(people_s[p]) ++ " majors in " ++ show(major_s[fix(major_inv[p])]) ++ " and returned " ++ show(returned[p]) ++ "\n"
  | p in 1..n
]
;

