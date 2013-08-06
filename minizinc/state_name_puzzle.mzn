% 
% State name puzzle in MiniZinc.
% 
% Solving the Rosetta Code State name puzzle
% http://rosettacode.org/wiki/State_name_puzzle
% """
% Background
%
% This task is inspired by Mark Nelson's DDJ Column "Wordplay" and one 
% of the weekly puzzle challenges from Will Shortz on NPR Weekend Edition 
% and originally attributed to David Edelheit.
%
% The challenge was to take the names of two U.S. States, 
% mix them all together, then rearrange the letters to form the names 
% of two different U.S. States (so that all four state names differ 
% from one another). What states are these?
%
% The problem was reissued on the Unicon Discussion Web which includes 
% several solutions with analysis. Several techniques may be helpful and 
% you may wish to refer to Gödel numbering, equivalence relations, and 
% equivalence classes. The basic merits of these were discussed in the 
% Unicon Discussion Web.
%
% A second challenge in the form of a set of fictitious new states 
% was also presented.
%
% Task:
% Write a program to solve the challenge using both the original 
% list of states and the fictitious list.
%
% Caveats:
%
%     case and spacing isn't significant - just letters (harmonize case)
%     don't expect the names to be in any order - such as being sorted
%     don't rely on names to be unique (eliminate duplicates - meaning 
%     if Iowa appears twice you can only use it once) 
%
% Comma separated list of state names used in the original puzzle:
%
%     "Alabama", "Alaska", "Arizona", "Arkansas",
%     "California", "Colorado", "Connecticut",
%     "Delaware",    
%     "Florida", "Georgia", "Hawaii",
%     "Idaho", "Illinois", "Indiana", "Iowa",
%     "Kansas", "Kentucky", "Louisiana",
%     "Maine", "Maryland", "Massachusetts", "Michigan",
%     "Minnesota", "Mississippi", "Missouri", "Montana",
%     "Nebraska", "Nevada", "New Hampshire", "New Jersey",
%     "New Mexico", "New York", "North Carolina", "North Dakota",
%     "Ohio", "Oklahoma", "Oregon",
%     "Pennsylvania", "Rhode Island",
%     "South Carolina", "South Dakota", "Tennessee", "Texas",
%     "Utah", "Vermont", "Virginia",
%     "Washington", "West Virginia", "Wisconsin", "Wyoming"
%
% Comma separated list of additional fictitious state names to be added 
% to the original (Includes a duplicate):
%
% "New Kory", "Wen Kory", "York New", "Kory New", "New Kory"
%
% """

% References mentioned above:
%  - Mark R. Nelson's puzzling column
%    http://drdobbs.com/windows/198701685
%  - Unicon Discussion
%    https://tapestry.tucson.az.us/twiki/bin/view/Main/StateNamesPuzzle


%
% Notes for this MiniZinc model:
%  * The basic approach is to use global cardinality count to ensure
%    that x1 and x2 has exactly the same distribution of letters.
%
%  * Since MiniZinc don't have good support for strings much of this
%    model consists of representation the names as ints.
%
%  * The original problem has - with symmetry breaking - one solution.
%
%  * The extra task (with the additional fictitious state names) has been
%    commented out in the current model.
%    This problem has 28 solutions (with symmetry breaking).
%    Without the duplicate detection (with symmetry breaking), there are 46 solutions.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_letters = 26;
int: a =  1; int: b =  2; int: c =  3; int: d =  4; int: e =  5; int: f =  6;
int: g =  7; int: h =  8; int: i =  9; int: j = 10; int: k = 11; int: l = 12;
int: m = 13; int: n = 14; int: o = 15; int: p = 16; int: q = 17; int: r = 18;
int: s = 19; int: t = 20; int: u = 21; int: v = 22; int: w = 23; int: x = 24;
int: y = 25; int: z = 26; 

array[1..num_letters] of string: letters = 
       ["a","b","c","d","e","f","g","h","i","j","k","l","m",
        "n","o","p","q","r","s","t","u","v","w","x","y","z"
    ];

int: max_len = 13;
int: num_states = 50;
% int: num_states = 55; % with added states
array[1..num_states, 1..max_len] of int: states = 
   array2d(1..num_states, 1..max_len,
   [
a,l,a,b,a,m,a,0,0,0,0,0,0,
a,l,a,s,k,a,0,0,0,0,0,0,0,
a,r,i,z,o,n,a,0,0,0,0,0,0,
a,r,k,a,n,s,a,s,0,0,0,0,0,
c,a,l,i,f,o,r,n,i,a,0,0,0,
c,o,l,o,r,a,d,o,0,0,0,0,0,
c,o,n,n,e,c,t,i,c,u,t,0,0,
d,e,l,a,w,a,r,e,0,0,0,0,0,
f,l,o,r,i,d,a,0,0,0,0,0,0,
g,e,o,r,g,i,a,0,0,0,0,0,0,
h,a,w,a,i,i,0,0,0,0,0,0,0,
i,d,a,h,o,0,0,0,0,0,0,0,0,
i,l,l,i,n,o,i,s,0,0,0,0,0,
i,n,d,i,a,n,a,0,0,0,0,0,0,
i,o,w,a,0,0,0,0,0,0,0,0,0,
k,a,n,s,a,s,0,0,0,0,0,0,0,
k,e,n,t,u,c,k,y,0,0,0,0,0,
l,o,u,i,s,i,a,n,a,0,0,0,0,
m,a,i,n,e,0,0,0,0,0,0,0,0,
m,a,r,y,l,a,n,d,0,0,0,0,0,
m,a,s,s,a,c,h,u,s,e,t,t,s,
m,i,c,h,i,g,a,n,0,0,0,0,0,
m,i,n,n,e,s,o,t,a,0,0,0,0,
m,i,s,s,i,s,s,i,p,p,i,0,0,
m,i,s,s,o,u,r,i,0,0,0,0,0,
m,o,n,t,a,n,a,0,0,0,0,0,0,
n,e,b,r,a,s,k,a,0,0,0,0,0,
n,e,v,a,d,a,0,0,0,0,0,0,0,
n,e,w,h,a,m,p,s,h,i,r,e,0,
n,e,w,j,e,r,s,e,y,0,0,0,0,
n,e,w,m,e,x,i,c,o,0,0,0,0,
n,e,w,y,o,r,k,0,0,0,0,0,0,
n,o,r,t,h,c,a,r,o,l,i,n,a,
n,o,r,t,h,d,a,k,o,t,a,0,0,
o,h,i,o,0,0,0,0,0,0,0,0,0,
o,k,l,a,h,o,m,a,0,0,0,0,0,
o,r,e,g,o,n,0,0,0,0,0,0,0,
p,e,n,n,s,y,l,v,a,n,i,a,0,
r,h,o,d,e,i,s,l,a,n,d,0,0,
s,o,u,t,h,c,a,r,o,l,i,n,a,
s,o,u,t,h,d,a,k,o,t,a,0,0,
t,e,n,n,e,s,s,e,e,0,0,0,0,
t,e,x,a,s,0,0,0,0,0,0,0,0,
u,t,a,h,0,0,0,0,0,0,0,0,0,
v,e,r,m,o,n,t,0,0,0,0,0,0,
v,i,r,g,i,n,i,a,0,0,0,0,0,
w,a,s,h,i,n,g,t,o,n,0,0,0,
w,e,s,t,v,i,r,g,i,n,i,a,0,
w,i,s,c,o,n,s,i,n,0,0,0,0,
w,y,o,m,i,n,g,0,0,0,0,0,0,

% added states
% n,e,w,k,o,r,y,0,0,0,0,0,0,
% w,e,n,k,o,r,y,0,0,0,0,0,0,
% y,o,r,k,n,e,w,0,0,0,0,0,0,
% k,o,r,y,n,e,w,0,0,0,0,0,0,
% n,e,w,k,o,r,y,0,0,0,0,0,0,  % duplicate
   ]); 

array[1..num_states] of string: states_str = 
[
"alabama",
"alaska",
"arizona",
"arkansas",
"california",
"colorado",
"connecticut",
"delaware ",
"florida",
"georgia ",
"hawaii",
"idaho",
"illinois",
"indiana",
"iowa",
"kansas",
"kentucky",
"louisiana",
"maine",
"maryland",
"massachusetts",
"michigan",
"minnesota",
"mississippi",
"missouri",
"montana",
"nebraska",
"nevada",
"new hampshire",
"new jersey",
"new mexico",
"new york",
"north carolina",
"north dakota",
"ohio",
"oklahoma",
"oregon",
"pennsylvania",
"rhode island",
"south carolina",
"south dakota",
"tennessee",
"texas",
"utah",
"vermont",
"virginia",
"washington",
"west virginia",
"wisconsin",
"wyoming",

% added states
% "new kory", 
% "wen kory", 
% "york new", 
% "kory new", 
% "new kory" % duplicate

];

% decision variables
int: num_get = 2;
array[1..num_get] of var 1..num_states: x1;     % two states
array[1..num_get] of var 1..num_states: x2;     % two other states
array[0..num_letters] of var 0..max_len*2: gcc; % global cardinality count

% solve satisfy;
solve :: int_search(
        x1 ++ x2, 
        input_order, 
        indomain_max, 
        complete) 
    satisfy;

% This is the basic approach
constraint
   alldifferent(x1 ++ x2) :: domain
   /\
   global_cardinality(
               [states[x1[J],I] | J in 1..2, I in 1..max_len], 
               array1d(0..num_letters, set2array(index_set(gcc))),
               gcc
   ) % :: domain
   /\
   global_cardinality(
               [states[x2[J],I] | J in 1..2, I in 1..max_len], 
               array1d(0..num_letters, set2array(index_set(gcc))),
               gcc
   ) :: domain
;


% handle duplicates
constraint
   forall(I,J in 1..num_get where I < J) (
      sum(K in 1..max_len) (bool2int(states[x1[I], K] != states[x1[J], K])) > 0
      /\
      sum(K in 1..max_len) (bool2int(states[x2[I], K] != states[x2[J], K])) > 0
   )
   /\
   forall(I,J in 1..num_get) (
      sum(K in 1..max_len) (bool2int(states[x1[I], K] != states[x2[J], K])) > 0
      /\
      sum(K in 1..max_len) (bool2int(states[x2[I], K] != states[x1[J], K])) > 0
   )
;

% symmetry breaking
constraint
   increasing(x1)
   /\
   increasing(x2)
   /\
   x1[1] < x2[1]
;

output [
  "x1: " ++ show(x1) ++ " " ++ show([states_str[fix(x1[I])] | I in 1..num_get] ) ++ "\n" ++
  "x2: " ++ show(x2) ++ " " ++ show([states_str[fix(x2[I])] | I in 1..num_get] ) ++ "\n"
]
++
[
  "gcc: " ++ show(gcc) ++ "\n"
]
++ ["\n"]
;

