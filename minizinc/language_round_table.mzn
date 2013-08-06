% 
% Languages at a round table in MiniZinc.
% 
% From Stack overflow
% "A prolog program that reflects people sitting at a round table"
% http://stackoverflow.com/questions/13305356/a-prolog-program-that-reflects-people-sitting-at-a-round-table
% """
% I've got a prolog homework to do: There are 5 persons sitting at a round table of 
% different nationalities (french, english, polish, italian, turkish). Each of them knows 
% only one other language other than their own. They sit at the round table in such a way that 
% each of them can talk with their 2 neighbors (with one neighbor they talk in their native 
% tongue and with the other in the single foreign language they know). 
%
% The english person knows italian, 
% the polish person knows french, 
% the turkish person doesn't know english. 
% 
% The question is what foreign language does the turkish person know?
% """

% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;

int: french  = 1;
int: english = 2;
int: polish  = 3;
int: italian = 4;
int: turkish = 5;

array[1..n] of string: s = ["French", "English", "Polish", "Italian", "Turkish"];

% decision variables
array[1..n] of var 1..n: native;
array[1..n] of var 1..n: foreign;
var 1..n: turk_foreign; % foreign language for the native Turkish

solve satisfy;


% p(native_language, foreign_language)
% i.e. the person with native_language speak foreign_language
predicate p(var int: nat, var int: f) =
  exists(j in 1..n) (
     native[j] = nat /\
     foreign[j] = f
  )
;


constraint

  % the native languages are distinct
  % (and - as it happen - so are the foreign languages)
  alldifferent(native)

  % They sit at the round table in such a way that each of them can talk with 
  % their 2 neighbors (with one neighbor they talk in their native tongue 
  % and with the other in the single foreign language they know). 
  /\ 
  forall(i in 0..n-1) (
    let {
      var {1+(i-1) mod n, 1+(i+1) mod n}: j
    } in
    native[1+(i mod n)] = foreign[j]
  )

  % % The english person knows italian, 
  /\ p(italian, english)

  % the polish person knows french, 
  /\ p(french, polish)

  % the turkish person doesn't know english. 
  /\ not(p(english,turkish))
  
  % The question is what foreign language does the turkish person know?
  /\ p(turkish, turk_foreign)

  % symmetry breaking: the French sits at position 1
  /\ native[french] = french
;

output 
[
  "native : " ++ show(native) ++ "\n" ++
  "foreign: " ++ show(foreign) ++ "\n" ++ 
  "turkish foreign language: " ++ show(s[fix(turk_foreign)]) ++ "\n\n"
]
++
[
  "native: " ++ show(s[fix(native[i])]) ++ " -> foreign: " ++ show(s[fix(foreign[i])]) ++ "\n"
  | i in 1..n

]
;