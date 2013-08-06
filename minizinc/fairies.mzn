% 
% Fairies problem in Minizinc
%

% From 
% Martin J. Chlond: Puzzle—O.R. with the Fairies
% Informs, Transactions on Education Volume 8, Issue 2 January 2008
% http://www.informs.org/site/ITE/article.php?id=58
% PDF: http://www.informs.org/site/ITE/downloadfile.php?i=3416a75f4cea9109507cacd8e2f2aefc
% 
% The original MathProg model had the following note:
% Model name : fairies.mod
% Description : Color Fairies puzzle
% Source         : Puzzling Adventures, Dennis E. Shasha
% Date written : 21/12/06
% Written by : Martin Chlond, Lancashire Business School
% Email          : mchlond@uclan.ac.uk

% 
% This Minizinc model was written by Hakan Kjellerstrand, 
% hakank@bonetmail.com,
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: f; % fairy
int: h; % child
int: c; % color
int: n; % night

set of int: fairy  =  1..f; % Cloe, Ariana, Oliviana, Anya, Caroline
set of int: child  =  1..h; % Tyler, Jordan, David
set of int: color  =  1..c; % silver, sage, gold, rose, turquoise, ivory, violet, emerald, earth
set of int: night =  1..n;

array[child, color] of 0..1: star; % star(j,k) = 1 if child j has star color k, else 0

array[child, night] of int: pearl; % pearl(j,m) = no of pearls received by child j during night m
array[fairy, night] of 0..1: visit; % visit(i,m) = 1 if fairy i visits children on night m, else 0

array[fairy, color] of var 0..1: x;  % x(i,k) = 1 if fairy i attracted to color k, else 0

solve satisfy;

constraint
  % pearls received by each child on each night consistent with visits and stars
  forall(j in child,m in night) (
        sum(i in fairy,k in color)
         (visit[i,m]*star[j,k]*x[i,k]) = pearl[j,m]
  )
  /\ % each fairy attracted to a single color
  forall(i in fairy) (
    sum(k in color) (x[i,k]) = 1
  )

  /\ % at least one fairy attracted to turquoise
  sum(i in fairy) (x[i,5]) >= 1

  /\ % one fairy attracted to earth
  sum(i in fairy) (x[i,9]) = 1
;

%
% data
%
f = 5;
c = 9;
h = 3;
n = 5;

star = [|0, 0, 0, 1, 1, 0, 1, 0, 0,
        |0, 1, 0, 0, 0, 1, 1, 0, 0,
        |0, 1, 0, 0, 0, 0, 1, 1, 0|];

pearl = [|1,0, 0, 1, 1,
         |1,2, 1, 1, 2,
         |2,2, 2, 0, 1|];

visit = [|1, 0, 0, 1, 1, 
         |0, 0, 1, 1, 0,
         |0, 1, 0, 1, 1,
         |1, 1, 1, 0, 1,
         |1, 1, 1, 0, 0|];


output [
 if j = 1 then "\n" else " " endif ++
   show(x[i,j])
 | i in fairy, j in color
] ++ ["\n"];

