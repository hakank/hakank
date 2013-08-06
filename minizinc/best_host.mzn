% 
% Best host (PuzzlOr problem) in MiniZinc.
% 
% From The PuzzlOr, February | Volume 38 | Number 1
% http://www.informs.org/ORMS-Today/Public-Articles/February-Volume-38-Number-1/THE-PUZZLOR
% """
% By John Toczek
%
% Hosting a dinner party requires several skills to pull off a successful 
% evening. One of your duties, aside from preparing dinner and selecting 
% the drinks, is to make sure your guests enjoy themselves.
%
% Figure 1 shows a dinner table with six seats for your guests. Some 
% guests, however, do not get along with each other. If two guests who 
% do not get along are seated next to each other, it will create conflict 
% at dinner. As host, you must arrange the guests in a seating order 
% that minimizes conflict.
%
% Andrew will only sit next to Dave and Frank; 
% Betty will only sit next to Cara and Erica; 
% Cara will only sit next to Betty and Frank; 
% Dave will only sit next to Andrew and Erica; 
% Erica will only sit next to Betty and Dave; 
% Frank will only sit next to Andrew and Cara. 
%
% [
%   Figure 1 shows the following arrangement:
%          
%               Andrew
%        Frank         Betty
%        Erica         Cara
%               Dave
% 
% ]
% 
% In the example seating arrangement above, there are three conflicts 
% (Andrew and Betty, Cara and Dave, Erica and Frank).
%
% Question:
%
% What seating arrangement will minimize the conflict at dinner?
% """
%
% Answer: There are 12 possible solutions with no conflict at all.
%
% By placing Andrew at position 1 (as symmetry breaking) there are 
% 2 possible solutions, where the 2nd solution is the mirror of the 1st.
%
% 1) Andrew Frank Cara Betty Erica Dave
%
%                Andrew
%        Dave           Frank
%        Erica          Cara
%                Betty
%
% 2) Andrew Dave Erica Betty Cara Frank
%    
%                Andrew 
%         Frank         Dave
%         Cara          Erica
%                Betty 
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6;

int: Andrew = 1;
int: Betty  = 2;
int: Cara   = 3;
int: Dave   = 4;
int: Erica  = 5;
int: Frank  = 6;

array[1..n] of  set of 1..n: prefs = 
[
 {Dave, Frank},   % Andrew
 {Cara, Erica},   % Betty
 {Betty, Frank},  % Cara
 {Andrew, Erica}, % Dave 
 {Betty, Dave},   % Erica
 {Andrew, Cara}   % Frank
 ]; 

% string version of the names
array[1..n] of string: name_str = ["Andrew", "Betty", "Cara", "Dave", "Erica", "Frank"];

% the seating arrangement
array[1..n] of var 1..n: x;

% number of fullfilled preferences
% var 0..2*n: num_prefs;


solve satisfy;
% solve maximize num_prefs;


% symmetry breaking
constraint 
   x[1] = Andrew
;


constraint 
   all_different(x)
;

%
% Just as a test: Count the number of fullfilled preferences
% Should be 12 for no conflicts.
%
% constraint
%    num_prefs = sum(i in 1..n) (
%         let {
%               int: a = if (i-1) mod n = 0 then n else (i-1) mod n endif,
%               int: b = if (i+1) mod n = 0 then n else (i+1) mod n endif,
%         } in
%         bool2int(x[a] in prefs[x[i]]) 
%         +
%         bool2int(x[b] in prefs[x[i]])
%    )
% ;

%
% Ensure that a person x[i] is sitting between 
% his/her preferred friends, prefs[x[i]].
%
constraint
   forall(i in 1..n) (
        let {
              int: a = if (i-1) mod n = 0 then n else (i-1) mod n endif,
              int: b = if (i+1) mod n = 0 then n else (i+1) mod n endif,
        } in
        x[a] in prefs[x[i]]
        /\
        x[b] in prefs[x[i]]
   )
;

output [
  "x: " ++ show(x) ++ "\n"  % ++
  % "num_prefs: " ++ show(num_prefs) ++ "\n" 
] ++ 
[
  show(name_str[fix(x[i])]) ++ " "
  | i in 1..n
] ++ 
["\n\n"] ++
[ "          " ++  show(name_str[fix(x[1])]) ++ "\n" ++
  "  " ++ show(name_str[fix(x[6])]) ++ "           " ++ show(name_str[fix(x[2])]) ++ "\n"  ++
  "  " ++ show(name_str[fix(x[5])]) ++ "           " ++ show(name_str[fix(x[3])]) ++ "\n"  ++
  "        " ++  show(name_str[fix(x[4])]) ++ "\n\n"

] ++
["\n"];