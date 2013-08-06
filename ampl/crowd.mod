/*
  Note: OBS! AMPL version 20091123 cannot handle this model anymore.

  Use instead ampl.older2 (version 20080307)

  Sat Jan  5 20:56:25 2008/hakank@bonetmail.com


1 - Queen, 2 - Bishop, 3 - Knight, 4 - Rook

    3    2    2    2    3    4    1    2
    3    3    4    3    1    3    0    2
    3    0    1    4    3    0    3    2
    1    0    0    3    0    3    4    2
    2    4    3    0    3    1    3    0
    2    3    0    3    4    3    0    1
    2    1    3    0    3    0    3    4
    4    3    0    1    2    2    2    2

Xpress-Mosel Model

model 'crowd'

!  Description  : The crowded board
!  Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.
!  Date written : Xpress-MP May 2000, Mosel 19/4/03
!  Written by   : M J Chlond

  uses 'mmxprs'

This model is too large for the AMPL demo.

cplex:
1 0 3 0 2 2 3 4
2 3 4 3 0 1 0 2
2 4 3 0 3 0 3 1
2 3 1 3 4 3 0 3
3 0 3 0 0 4 1 2
4 3 0 1 0 3 0 2
2 1 3 4 3 3 3 2
2 3 2 2 1 3 4 2

bonmin:
1 0 2 2 3 0 3 4
2 3 0 3 4 3 1 2
3 0 3 1 3 4 3 2
4 3 0 3 0 1 0 2
2 4 3 0 0 0 3 1
2 1 4 3 0 3 3 3
2 0 3 4 1 0 3 2
2 3 1 3 2 2 4 2

cbc:
2 2 2 0 2 0 1 4
4 1 0 3 0 3 0 2
3 4 3 0 3 1 3 2
2 3 1 3 0 3 4 3
1 0 0 0 3 4 3 2
2 3 0 1 4 3 0 3
2 3 3 4 3 0 3 1
2 3 4 2 1 2 2 3

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param size := 8;
param piece := 4;    # 1 - Queen, 2 - Bishop, 3 - Knight, 4 - Rook

set S := 1..size+4;
set P := 1..piece+4;
set  R := 3..size+2;  # real part of board
# Note: P is 1..8, but the Mosel model defines N for 1..4
# I defaults to 0
param N{P} default 0; #  := {8,14,21,8};
var x{S,S,P} binary; 

minimize any: 
        sum{i in R,j in R,k in P} x[i,j,k];

s.t.  nump{k in P}:
        sum{i in R,j in R} x[i,j,k] = N[k];
    
s.t.   onep{i in R,j in R}:
        sum{k in P} x[i,j,k] <= 1;

# No queens attack each other
s.t.  qrow{i in R}:
        sum{j in R} x[i,j,1] <= 1;

s.t.  qcol{j in R}:
        sum{i in R} x[i,j,1] <= 1     ;

s.t.  qdia{i in 2..size+3}:
        sum{k in 1..i} x[k,i-k+1,1] <= 1;

s.t. qdib{j in 1..size+3}:
        sum{k in j..size+4} x[k,size+4-k+j,1] <= 1  ;

s.t.  qdic{j in 1..size+3}:
        sum{k in 1..size-j+5} x[k,j+k-1,1] <= 1;

s.t. qdid {i in 2..size+3}:
        sum{k in i..size+4} x[k,k-i+1,1] <= 1;

  # No bishops attack each other
s.t.  bda{i in 2..size+3}:
        sum{k in 1..i} x[k,i-k+1,2] <= 1;
s.t.  bdb{j in 1..size+3}:
        sum{k in j..size+4} x[k,size+4-k+j,2] <= 1;
s.t.  dbc{j in 1..size+3}:
        sum{k in 1..size-j+5} x[k,j+k-1,2] <= 1;
s.t.  bdd{i in 2..size+3}:
        sum{k in i..size+4} x[k,k-i+1,2] <= 1;

  # No rooks attack each other
s.t.  rrow{i in R}:
        sum{j in 3..size+2} x[i,j,4] <= 1;
s.t.  rcol{j in R}:
        sum{i in 3..size+2} x[i,j,4] <= 1;

  # a(i,j,3] = 0 if square {i,i} attacked by knight 
s.t.  knta{i in R,j in R}:
        x[i-2,j-1,3] + x[i-1,j-2,3] + x[i+1,j-2,3] + x[i+2,j-1,3] + 
        x[i+2,j+1,3] + x[i+1,j+2,3] + x[i-1,j+2,3] + x[i-2,i+1,3] + 99*x[i,j,3] <= 99;

# Dummy squares not occupied 
s.t.  setzero: sum{i in 1..size+4,j in 1..size+4,k in 1..4 : i < 3 or i > size+2 or j < 3 or j > size+2} x[i,j,k] =  0;

# /*
data;

param N := 
        1 8,
        2 14,
        3 21,
        4 8
;
# */

#option presolve 0;
#option cplex_options "sensitivity";
option solver cplex;
# option solver gurobi;
# option solver bonmin;
# option solver cbc;
#option solver donlp2;
#option solver gjh;
# option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
# option solver LaGO;
# option solver loqo;
# option solver lpsolve;
# option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display x;
display _obj;
#display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;


for{i in 1..size} {
    for{j in 1..size} {
      printf "%d ", (sum{k in P} k*x[i+2,j+2,k]);
    }
    printf "\n";
}

/*
data;

param N := 
        1 8
        2 14
        3 21
        4 8
;


end;
*/
