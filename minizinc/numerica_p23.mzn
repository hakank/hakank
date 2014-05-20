% 
% Numerica problem page 23 in MiniZinc.
% 
% A Robot Kinematic Application
%
% JaCoP:
%    float_search with 0.01: 45 solutions
%    solve satisfy: 46 solutions (22 distinct) (21s)
% Gecode ignores 0.01 and give a huge number of solutions 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 6;
float: v = pow(10.0,8.0);
% float: v = pow(10.0,2.0);

% array[1..n] of var -v..v: s;
%array[1..n] of var -v..v: c;

array[1..n] of var float: s;
array[1..n] of var float: c;


% solve satisfy;
solve :: float_search(c ++ s, 0.000000001, input_order, indomain_split, complete) satisfy;


constraint
  forall(i in 1..n) (
      s[i] >= 0.0 /\
      c[i] >= 0.0 /\
      s[i]*s[i] + c[i]*c[i] = 1.0
  )  
  /\
  c[5]*s[6]*(s[2]-s[3]-s[4])+c[6]*(c[2]+c[3]+c[4])= 0.4077
  /\
  c[1]*s[5]*(c[2]+c[3]+c[4])+s[1]*c[5]=1.9115
  /\
  s[5]*(s[2]+s[3]+s[4]) = 1.9791
  /\
  c[1]*(3.0*c[2]+2.0*c[3]+c[4]) = 4.0616
  /\
  s[1]*(3.0*c[2]+2.0*c[3]+c[4]) = 1.7172
  /\
  3.0*s[2]+ 2.0*s[3] + s[4] = 3.9701
  
;

output [
  "s:" ++ show(s) ++ "\n" ++
  "c:" ++ show(c) ++ "\n"
];

