% 
% GCD and LCM in MiniZinc.
%
% Not very effective for larger values...
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 100;

var 1..n: a;
var 1..n: b;
% var 1..n: gg;
var 1..n*n: ll;

%
% gcd(x,y,g)
% g is the gcd of x and y
%
predicate gcd(var int: x, var int: y, var int: g) =
  let {
    int: p = min(ub(x),ub(y))
  } in
   exists(i in 1..p) (
       x mod i = 0 /\ y mod i = 0
       /\
       forall(j in i+1..p) (
           not(x mod j = 0 /\ y mod j = 0)
       )
       /\
       g = i
   )
;

%
% lcm(x,y,cm)
% cm is the lcm of x and y
%
predicate lcm(var int: x, var int: y, var int: cm) =
    let {
       int: p = min(ub(x),ub(y)),
       var 1..p: g
    } in
    gcd(x,y,g)
    /\
    cm = x*y div g
;

solve satisfy;

constraint
   % a = 121 /\ % /\  b = 7 /\
   % /\ 
   % gg = 17 /\
   ll = 12 /\
   % gcd(a,b,gg)
   % /\
   lcm(a,b,ll)
;

output 
[
  % "gcd: " ++ show([a,b,gg]) ++ "\n" ++
  "lcm: " ++ show([a,b,ll]) ++ "\n" 
];
