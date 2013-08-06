/*

  Traffic lights problem in AMPL+CP.

  CSPLib problem 16
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
  """
  Specification:
  Consider a four way traffic junction with eight traffic lights. Four of the traffic 
  lights are for the vehicles and can be represented by the variables V1 to V4 with domains 
  {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are 
  for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.
  
  The constraints on these variables can be modelled by quaternary constraints on 
  (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples 
  {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.

  It would be interesting to consider other types of junction (e.g. five roads 
  intersecting) as well as modelling the evolution over time of the traffic light sequence. 
  ...

  Results
  Only 2^2 out of the 2^12 possible assignments are solutions.
  
  (V1,P1,V2,P2,V3,P3,V4,P4) = 
     {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
     [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}


  The problem has relative few constraints, but each is very tight. Local propagation 
  appears to be rather ineffective on this problem.    
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n := 4;
set colors;
param color_id{colors};

param allowed{1..n, 1..n} symbolic in colors;


# Cars
var V{1..n} >= 1 <= 4 integer;

# Pedestrians
# We restrict the valid values {1,3} in a constraint (c2).
var P{1..n} >= 1 <= 3 integer; # Pedestrians


#
# constraints
#
s.t. c1{i in 1..n, j in 1..n: j = ((1+i) mod 4)}:
        exists{a in 1..n} (
           color_id[allowed[a,1]] = V[i] and
           color_id[allowed[a,2]] = P[i] and
           color_id[allowed[a,3]] = V[j] and
           color_id[allowed[a,4]] = P[j]
        )
;

# P[i..n] can only be {1,3}, i.e. {r,g}.
s.t. c2{i in 1..n}: P[i] = 1 || P[i] = 3;

# s.t. c3: V[1] = 3;
# s.t. c3: V[1] = 1;

data;

set colors = r g ry y;

param color_id :=
   r  1
   ry 2
   g  3
   y  4;


param allowed: 1 2 3 4 :=
  1   r  r  g  g
  2  ry  r  y  r
  3   g  g  r  r
  4   y  r ry  r
;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=60";

# option solver ilogcp;

solve;
# write gtest;

for {i in 1..n} {
    printf "%d %d  ", V[i], P[i];
};
printf "\n";

display V;
display P;