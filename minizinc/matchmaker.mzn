% 
% Matchmaker problem (PuzzlOR) in MiniZinc.
% 
% 
% http://puzzlor.editme.com/Matchmaker:
% """
% The Matchmaker was an honored profession in past cultures, serving the valuable 
% purpose of pairing off men and women in hopes of a long and successful relationship.  
% The Matchmaker would carefully consider the characteristics of each partner to 
% determine which pairs would be compatible. 
%
% Figure 1 shows six men and six women each with varying hair color and eye color.  
% A man or woman will only accept a partner that has at least one of these traits in 
% common.  For example, Man A and Woman 5 would make a matching pair because they 
% have at least one trait in common (same hair color).  However Man A and Woman 1 would 
% not make a matching pair because they do not have any traits in common.
% 
% Question:  What pairings of men and women allow for everyone to have a partner with 
% at least one trait in common?  (There are several correct answers.)
% """

% Women   Hair color  Eye color
% 1       Yellow      Brown       
% 2       Red         Green
% 3       Black       Blue
% 4       Black       Brown
% 5       Red         Brown
% 6       Black       Brown

% Men   Hair color  Eye color
% A       Red         Green
% B       Yellow      Blue
% C       Yellow      Brown  
% D       Red         Green
% E       Black       Green
% F       Yellow      Blue


% From the wording "purpose of pairing off men and women" I assume that the object
% is to pair a man with a women.

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Solution(s):
%
% One optimal solution is the following of total 7 common traits:
% 
%   total: 7
%   x: [6, 1, 2, 5, 4, 3]
%   Woman 1 matches man F  Hair: Yellow <-> Yellow+ Eyes: Brown <-> Blue- num_matches: 1
%   Woman 2 matches man A  Hair: Red    <-> Red+    Eyes: Green <-> Green+ num_matches: 2
%   Woman 3 matches man B  Hair: Black  <-> Yellow- Eyes: Blue  <-> Blue+ num_matches: 1
%   Woman 4 matches man E  Hair: Black  <-> Black+  Eyes: Brown <-> Green- num_matches: 1
%   Woman 5 matches man D  Hair: Red    <-> Red+    Eyes: Brown <-> Green- num_matches: 1
%   Woman 6 matches man C  Hair: Black  <-> Yellow- Eyes: Brown <-> Brown+ num_matches: 1
%
% There are in total 8 solutions to this problem, all with an optimal 7 traits that match.
% Here are the other 7 solutions:
%
% Solution #2
% Woman 1 match man B  
% Woman 2 match man A  
% Woman 3 match man F  
% Woman 4 match man E  
% Woman 5 match man D  
% Woman 6 match man C  
%
% Solution #3
% Woman 1 match man F  
% Woman 2 match man A  
% Woman 3 match man B  
% Woman 4 match man C  
% Woman 5 match man D  
% Woman 6 match man E  
%
% Solution #4
% Woman 1 match man B  
% Woman 2 match man A  
% Woman 3 match man F  
% Woman 4 match man C  
% Woman 5 match man D  
% Woman 6 match man E  
% 
% Solution #5
% Woman 1 match man F  
% Woman 2 match man D  
% Woman 3 match man B  
% Woman 4 match man C  
% Woman 5 match man A  
% Woman 6 match man E  
% 
% Solution #6
% Woman 1 match man B  
% Woman 2 match man D  
% Woman 3 match man F  
% Woman 4 match man C  
% Woman 5 match man A  
% Woman 6 match man E  
% 
% Solution #7
% Woman 1 match man F  
% Woman 2 match man D  
% Woman 3 match man B  
% Woman 4 match man E  
% Woman 5 match man A  
% Woman 6 match man C  
% 
% Solution #8
% Woman 1 match man B  
% Woman 2 match man D  
% Woman 3 match man F  
% Woman 4 match man E  
% Woman 5 match man A  
% Woman 6 match man C  

% 
% As we see the choices of the possible matching pairs are quite restricted:
% 
% Women 1 can only be matched with man 2 and 6
% Women 2 can only be matched with man 1 and 4
% Women 3 can only be matched with man 2 and 6
% Women 4 can only be matched with man 3 and 5
% Women 5 can only be matched with man 1 and 4
% Women 6 can only be matched with man 3 and 5
%


include "globals.mzn"; 

int: n = 6;

int: Yellow = 1;
int: Brown  = 2;
int: Red    = 3;
int: Green  = 4;
int: Black  = 5;
int: Blue   = 6;

array[1..n] of string: color_str = ["Yellow", "Brown", "Red", "Green", "Black", "Blue"];

array[1..n, 1..2] of int: women = array2d(1..n, 1..2,
     [
       % Hair       Eyes
       Yellow,      Brown, % 1
       Red,         Green, % 2
       Black,       Blue,  % 3
       Black,       Brown, % 4
       Red,         Brown, % 5
       Black,       Brown, % 6
     ]);

array[1..n, 1..2] of int: men = array2d(1..n, 1..2,
     [
       % Hair       Eyes
       Red,         Green,  % A
       Yellow,      Blue,   % B
       Yellow,      Brown,  % C
       Red,         Green,  % D
       Black,       Green,  % E
       Yellow,      Blue,   % F
     ]);

array[1..n] of string: men_str = ["A","B","C","D","E","F"];


% decision variable
% Here we try to match the women 1..6 with some of the men A..F
array[1..n] of var 1..n: x;

% total matchings (to maximize)
array[1..n] of var 0..2: num_matches; % for presentation
var 0..2*n: total; 


% solve maximize total;
solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) maximize total;



constraint
   alldifferent(x)
   /\
   forall(i in 1..n) (
      % count the matches
      num_matches[i] = bool2int( women[i, 1] = men[x[i], 1]) + bool2int( women[i, 2] = men[x[i], 2]) 
      /\ 
      % the must be at least one common trait in a match
      num_matches[i] >= 1 
   )
   /\
   total = sum(num_matches)    
;


output [
  "total: " ++ show(total) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
] ++ [
  let {
    int: t = fix(x[i]) 
  } in
    "Woman " ++ show(i) ++ " match man " ++ show(men_str[t]) ++ "  " ++ "\n"
  | i in 1..n
] ++ [
  let {
    int: t = fix(x[i])
  }
  in
 "Woman " ++ show(i) ++ " matches man " ++ show(men_str[t]) ++ "  " ++
 "Hair: " ++ show(color_str[women[i,1]]) ++ " <-> " ++ show(color_str[men[t,1]]) ++ 
     if women[i,1] == men[t,1] then "+" else "-" endif ++
 " Eyes: " ++ show(color_str[women[i,2]]) ++ " <-> " ++ show(color_str[men[t,2]]) ++ 
     if women[i,2] == men[t,2] then "+" else "-" endif ++ " num_matches: " ++ 
 show(num_matches[i]) ++  "\n"
  | i in 1..n
] 
 ++ ["\n"]
;

