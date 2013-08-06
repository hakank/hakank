% 
% Two-dimensional constrained channels in MiniZinc.
% 
% From 
% David McKay: 
% "Information Theory, Inference, and Learning Algorithms"
% page 262
% http://www.inference.phy.cam.ac.uk/mackay/itila/book.html
% """
% These observations about crosswords were first made by Shannon (1948); I
% learned about them from Wolf and Siegel (1998). The topic is closely related
% to the capacity of two-dimensional constrained channels. An example of a
% two-dimensional constrained channel is a two-dimensional bar-code, as seen
% on parcels.
%
% Excercise 18.2
% A two-dimensional channel is defined by the constraint that,
% of the eight neighbours of every interior pixel in an N × N rectangular
% grid, four must be black and four white. (The counts of black and white
% pixels around boundary pixels are not constrained.)
% """
%

%
% From n = 13 (and m=4) and on it seems that there are always 78 solutions.
%

% N #solutions
% (1  2)
% (2 16)
% 3 140
% 4 558
% 5 748
% 6 602
% 7 262
% 8 202
% 9 166
% 10 134
% 11 126
% 12 94
%
% 13 78  <- from here on it seems to be alway 78 solutions
% 14 78
% 15 78
% 16 78
% .....
% 
% 26 78
%
% .... 
% 46 78
% ...
% 
% 
% This sequence is not in OEIS:
% 2, 16, 140, 558, 748, 602, 262, 202, 166, 134, 126, 94, 78

%
% For m = 2 or 6  (i.e. 2|6 1's as neighbour) it seems that 8 is the limit
% 
% 1: %%  solutions:     2
% 2: %%  solutions:     16
% 3: %%  solutions:     56
% 4: %%  solutions:     144
% 5: %%  solutions:     238
% 6: %%  solutions:     52
% 7: %%  solutions:     8
% 8: %%  solutions:     8
% 9: %%  solutions:     8
% 10: %%  solutions:     8
% 11: %%  solutions:     8
% 12: %%  solutions:     8
% 13: %%  solutions:     8
% 14: %%  solutions:     8
% 15: %%  solutions:     8
% 16: %%  solutions:     8
% 17: %%  solutions:     8
% 18: %%  solutions:     8
% 19: %%  solutions:     8
% 20: %%  solutions:     8
% 21: %%  solutions:     8
% 22: %%  solutions:     8
% 23: %%  solutions:     8
% 24: %%  solutions:     8

%
% More general: What are the limits for each m (number of 1's as neighbours)?
%
% m: limit
% 1   0
% 2   8
% 3   0
% 4  78
% 5   0
% 6   8
% 7   0
% 8   1
% 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 13;
% int: n;
int: m = 4;

% decision variables
array[1..n, 1..n] of var 0..1: x;


solve satisfy;
% solve :: int_search([x[i,j] | i,j in 1..n], occurrence, indomain_max, complete) satisfy;

constraint
  forall(i, j in 2..n-1) (
    m = sum(a, b in {-1,0,1} where 
            i+a >= 1 /\ j+b >= 1 /\
            i+a <= n /\ j+b <= n /\
            not(a = 0 /\ b = 0) % don't count this cell
         )
           (x[i+a,j+b])
  )
;

output 
[
   if j = 1 then "\n" else "" endif ++
     if fix(x[i,j]) = 1 then
       "X" 
     else 
       "_"
     endif
   | i,j in 1..n
]
 ++ ["\n"]
;

