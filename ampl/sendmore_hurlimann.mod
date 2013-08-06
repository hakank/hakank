/*

 Translating from Tony Hurlimann, sendmore.lpl:

MODEL SendMore ;
  SET i := /1:8/;
  DISTINCT VARIABLE x{i} [0,9]     ;
  BINARY VARIABLE c1; c2; c3; c4   ; 
  STRING PARAMETER letter{i} := 
    ['S','E','N','D','M','O','R','Y']
      ; 
  CONSTRAINT
    R2: x[1]>0 AND x[5]>0 ;
    R3: c1 = x[5]         ;
    R4: c2 + x[1]+x[5] = x[6]+10*c1   ;
    R5: c3 + x[2]+x[6] = x[3]+10*c2   ;
    R6: c4 + x[3]+x[7] = x[2]+10*c3   ;
    R7:      x[4]+x[2] = x[8]+10*c4   ;
  MAXIMIZE any: x[1];
  WRITE 'The letter %s is %1d\n' : ROW{i} (letter, x);
END

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

set ints := 1..8;

# Note: LPL has DISTINCT constraint.
# We must program this explicitly.
#
#  DISTINCT VARIABLE x{i} [0,9]     ;
var x{ints} integer >= 0 <=9; # [0,9]     ;
var c1 binary; 
var c2 binary; 
var c3 binary; 
var c4 binary; 
param letter{ints} symbolic;

param M:=10; # for the all diff trick
var y{ints,ints}  binary;

maximize any: x[1];

s.t.
    # R2: x[1]>=1 and x[5]>=1 ;
        R2_1: x[1]>=1;
        R2_2: x[5]>=1 ;
        R3: c1 = x[5]                  ;
        R4: c2 + x[1]+x[5] = x[6]+10*c1;
        R5: c3 + x[2]+x[6] = x[3]+10*c2;
        R6: c4 + x[3]+x[7] = x[2]+10*c3;
        R7:      x[4]+x[2] = x[8]+10*c4;
        
        all_differentA1{u in ints, v in ints}:
        M*y[u,v]       + (x[u] - x[v]) >= (if u <> v then 1);

        all_differentA2{u in ints, v in ints}:
        M*(1 - y[u,v]) + (x[v] - x[u]) >= (if u <> v then 1);

data;
param letter := 
        1 'S',
        2 'E',
        3 'N',
        4 'D',
        5 'M',
        6 'O',
        7 'R',
        8 'Y'
;

option show_stats 1;
option solver cplex;
#option solver bonmin;
option solver lpsolve;

solve;

display  x;
# 'The letter %s is %1d\n' : ROW{i} (letter, x);
for{u in ints} {
        printf "%d: %d\n", u, x[u];
}
printf "\n";


