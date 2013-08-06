% 
% Rock Start Dressing Problem in MiniZinc.
% 
% From the presentation
% http://www.cse.unsw.edu.au/~cs4418/2010/Lectures/Constraints2.ppt 
% (From the course 
%   "Knowledge Representation and Reasoning COMP4418"
%    http://www.cse.unsw.edu.au/~cs4418/2010/index.html
% )
%
% """
% A rock star has
%   - red, green, orange and blue shirts
%   - pink and yellow ties
%   - black, khaki, purple and blue pants 
%
%   - Fashion sense:
%     A yellow tie only goes with red, green or blue shirts
%     A pink tie only goes with blue shirts 
%     etc
%  Find a satisfactory outfit.
% """

% Here are the exact Fashion sense requirements:
% 
% Pants Shirt
% -----------
% khaki  green
% blue   red
% purple orange
% black  blue
% 
% Shirt  Tie
% ----------
% red    yellow
% green  yellow
% blue   yellow
% blue   pink
% 
% Tie    Pants
% ------------
% yellow blue
% yellow black
% pink   black
% pink   purple
% pink   khaki
%
 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: red    = 1;
int: green  = 2;
int: orange = 3;
int: blue   = 4;
int: pink   = 5;
int: yellow = 6;
int: black  = 7;
int: khaki  = 8;
int: purple = 9;

array[1..9] of string: colors = ["red","green","orange","blue","pink","yellow","black","khaki","purple"];

set of int: shirt_domain = {red, green, orange, blue};
set of int: tie_domain   = {pink, yellow};
set of int: pants_domain = {black, khaki, purple, blue};

% requirements
array[1..4, 1..2] of int: pants_shirt = array2d(1..4, 1..2, 
        [khaki,  green,
         blue,   red,
         purple, orange,
         black,  blue]);

array[1..4, 1..2] of int: shirt_tie = array2d(1..4, 1..2, 
        [red,    yellow,
         green,  yellow,
         blue,   yellow,
         blue,   pink]);
 
array[1..5, 1..2] of int: tie_pants = array2d(1..5, 1..2, 
        [yellow, blue,
         yellow, black,
         pink,   black,
         pink,   purple,
         pink,   khaki]);


% decision variables
var shirt_domain: shirt;
var tie_domain  : tie;
var pants_domain: pants;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


% Collect the different fashion sense into table/2 constraints.
constraint
   table([pants, shirt], pants_shirt)  /\ 
   table([shirt, tie  ], shirt_tie  )  /\ 
   table([tie  , pants], tie_pants  ) 
;

output [
  "pants: " ++ show(colors[fix(pants)]) ++ "\n" ++
  "shirt: " ++ show(colors[fix(shirt)]) ++ "\n" ++
  "tie  : " ++ show(colors[fix(tie)])   ++ "\n"
]
++ ["\n"]
;

