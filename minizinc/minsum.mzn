% 
% Minsum problem in MiniZinc.
% 
% From Alma-0, sumsum.a0:
% """
% The problem is from 
% @article{Gri82,
% Author		= { D.  Gries },
% Title		= { A Note on a Standard Strategy for Developing Loop Invariants and Loops },
% Journal		= scp,
% Volume		= 2,
% Year		= 1982,
% Pages		= {207--214}
% }
%
% The description follows 
% @book{AO97,
%       author	= { K. R.  Apt and E. -R.  Olderog },
%       title = "Verification of Sequential and 
%                Concurrent Programs",
%       publisher = "Springer-Verlag",
%       edition = "second",
%       address = "New York",
%       year = 1997
%       }
%
% Consider an array a[1..N] of type INTEGER. By a section of a we mean a
% fragment of it of the form a[i:j] where 1 <= i <= j <= N.  The sum of
% a section a[i:j] is the sum of its elements.  A minimum-sum of
% a[1..N]$ is a section a[i:j] such that the sum of a[i:j] is minimal
% among all subsections of a[0..N].
%
% For example, the minimum-sum section of a[1..5]=(5,-3,2,-4,1)$ is
% $a[2:4]=(-3,2,-4)$ and its sum is -5.  The two minimum-sum sections of
% a[1..5]=(5,2,5,4,2) are $a[2:2]$ and $a[5:5]$.
%
% The problem is to find all mimimum-sum sections and to compute their sum.
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% int: n = 5;
% array[1..n] of int: L = [5,-3,2,-4,1];
% % array[1..n] of int: L = [5,2,5,4,2];

int: n = 100;
array[1..n] of int: L = [-3,-20,22,-7,43,34,-19,48,-39,-35,40,-45,37,-5,-32,-37,-29,28,41,-34,-14,-26,16,42,-24,28,-44,1,-21,-38,17,28,-47,24,-16,-24,-23,-31,33,-25,4,22,-10,-20,45,-16,-26,32,-36,43,-34,-8,-31,-31,44,-35,-37,17,4,-45,9,-20,-5,-34,-48,40,-2,-25,-24,41,44,50,16,-15,-34,-31,37,-15,-44,-6,-12,15,-44,-38,13,-19,-9,40,16,47,-41,23,31,-6,39,7,-13,-4,-44,-36];

int: abs_max = sum([abs(L[i]) | i in 1..n]);

% decision variables
var 1..n: start;
var 1..n: end;
var -abs_max..abs_max: minsum;

% solve minimize minsum;
solve :: int_search([start,end], first_fail, indomain_min, complete) minimize minsum;

%% Using exists
% constraint
%    end >= start 
%    /\
%    exists(i, j in 1..n) (
%      i = start /\ j = end /\
%      minsum = sum([L[k] | k in i..j])
%    )
% ;

% alternative version
constraint
   end >= start 
   /\
   minsum = sum([L[k]*bool2int(k>=start /\ k<= end) | k in 1..n])
;


output [
  show([start,end,minsum]) ++ "\n" ++
  "L: " ++ show([L[i] | i in fix(start)..fix(end)])
];

