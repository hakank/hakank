% 
% Hanging weights puzzle in MiniZinc.
% 
% From 
% "Using LINQ to solve puzzles"
% http://blogs.msdn.com/lukeh/archive/2007/03/19/using-linq-to-solve-puzzles.aspx
% """
% Here's a puzzle similar to the one in the puzzle hunt.  The diagram 
% below is a bunch of weights (A-M) hanging from a system of bars.  
% Each weight has an integer value between 1 and 13, and the goal is 
% to figure out what each weight must be for the the diagram below to 
% balance correctly as shown: 
%
%                          |
%                          |
%              +--+--+--+--+--+--+--+
%              |                    |
%              |                    |
%           +--+--+--+--+--+        |
%           |     L        M        |
%           |                       |
%  +--+--+--+--+--+--+     +--+--+--+--+--+
%  H              |  I     |  J        K  |
%                 |        |              |
%        +--+--+--+--+--+  |     +--+--+--+--+--+
%        E              F  |     G              |
%                          |                    |
%              +--+--+--+--+--+  +--+--+--+--+--+--+
%              A              B  C                 D
%
% The rules for this kind of puzzle are: 
% (1) The weights on either side of a given pivot point must be equal, 
%     when weighted by the distance from the pivot, and 
% (2) a bar hanging beneath another contributes it's total weight as 
%     through it were a single weight.  For instance, the bar on the bottom 
%     right must have 5*C=D, and the one above it must have 3*G=2*(C+D).
% """
% 
%
% The minizinc solver (the only one that supports output[]), gives the 
% following solution:
%                           |
%                           |
%               +--+--+--+--+--+--+--+
%               |                    |
%               |                    |
%            +--+--+--+--+--+        |
%            |     7        5        |
%            |                       |
%   +--+--+--+--+--+--+     +--+--+--+--+--+
%  11              |  1     |  4        13 |
%                  |        |              |
%         +--+--+--+--+--+  |     +--+--+--+--+--+
%         6              9  |     8              |
%                           |                    |
%               +--+--+--+--+--+  +--+--+--+--+--+--+
%               3              12 2                 10


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
set of 1..13: R = 1..13;
var R: a;
var R: b;
var R: c;
var R: d;
var R: e;
var R: f;
var R: g;
var R: h;
var R: i;
var R: j;
var R: k;
var R: l;
var R: m;
var int: total = a+b+c+d+e+f+g+h+i+j+k+l+m;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   all_different([a,b,c,d,e,f,g,h,i,j,k,l,m]) /\
   4 * a = b /\ 
   5 * c = d /\ 
   3 * e = 2 * f /\ 
   3 * g = 2 * (c + d) /\ 
   3 * (a + b) + 2 * j = k + 2 * (g + c + d) /\ 
   3 * h = 2 * (e + f) + 3 * i /\ 
   (h + i + e + f) = l + 4 * m /\ 
   4 * (l + m + h + i + e + f) = 3 * (j + k + g + a + b + c + d)
;

output[

"                          |" ++ "\n" ++
"                          |"++ "\n" ++
"              +--+--+--+--+--+--+--+" ++ "\n" ++
"              |                    |" ++ "\n" ++
"              |                    |"++ "\n" ++
"           +--+--+--+--+--+        |"++ "\n" ++
"           |     "++show(l)++"        " ++show(m)++"        |"++ "\n" ++
"           |                       |"++ "\n" ++
"  +--+--+--+--+--+--+     +--+--+--+--+--+"++ "\n" ++
" "++show(h)++"              |  "++show(i)++"     |  "++show(j)++"        "++show(k)++" |"++ "\n" ++
"                 |        |              |"++ "\n" ++
"        +--+--+--+--+--+  |     +--+--+--+--+--+"++ "\n" ++
"        "++show(e)++"              "++show(f)++"  |     "++show(g)++"              |"++ "\n" ++
"                          |                    |"++ "\n" ++
"              +--+--+--+--+--+  +--+--+--+--+--+--+"++ "\n" ++
"              "++show(a)++"              "++show(b)++" "++show(c)++"                 "++show(d)++"\n"
];

