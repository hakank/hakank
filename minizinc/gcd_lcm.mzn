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
var 1..n: gg;
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

predicate lcm_array(array[int] of var int: a, var int: b) =
    let {
      int: lba = min(index_set(a)),
      int: uba = max(index_set(a)),
      array[1..length(a)] of var lb_array(a)..ub_array(a): tmp
    } in
    tmp[1] = a[1] 
    /\
    forall(i in lba+1..uba) (
       tmp[i] = lcm(tmp[i-1], a[i], tmp[i])
    )
    /\
    b = tmp[uba]
;

solve satisfy;

constraint
   % a = 10 /\ b = 7 /\
   % /\ 
   % gg = 17 /\
   % ll = 12 /\ 

   % ll mod 111 = 0 /\
   
   a < b /\
   gg > 1 /\
   gcd(a,b,gg)
   /\
   lcm(a,b,ll)
;

output 
[
  "gcd: " ++ show([a,b,gg]) ++ "\n" ++
  "lcm: " ++ show([a,b,ll]) ++ "\n" 
];
