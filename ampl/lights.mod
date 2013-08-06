/*
  Sat Jan  5 22:20:45 2008/hakank@bonetmail.com

The actual solution will depend on the starting configuration. A non-IP method
to obtain a specific solution is as follows.
<p>
Create a four by four table with a one representing each red square and a zero
representing each white square.
<p>
Without touching the puzzle, figure out what the new configuration would be if
you had clicked every red square. Create a second table, as above, based on this
configuration.
<p>
Add the two tables, element by element, into a third table.
<p>
The cells in the third table that contain the number one are the cells to click
in order to complete the puzzle in the minimum number of moves.


Xpress-Mosel Model

model 'lights'

! Description  : Lights on puzzle
! Source       : Unknown
! Date written : Xpress-MP 5/4/97, Mosel 17/4/03
! Written by   : M J Chlond

  uses 'mmxprs'

Xpress:
1 1 1 0 
1 1 1 1 
1 0 0 1 
1 1 0 1 

cplex:
1 1 1 0
1 1 1 1
1 0 0 1
1 1 0 1

glpk:
1 1 1 0
1 1 1 1
1 0 0 1
1 1 0 1

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


param n := 4;

set N := 1..n;
param r{N,N};
var x{N,N} binary;
var d{N,N} integer >= 0;

minimize moves: sum{i in N, j in N} x[i,j];

s.t. con{i in N, j in N}:
        sum{l in N} x[i,l] +
           sum{k in N : k <> i} x[k,j] = 2*d[i,j]+r[i,j];


# /*
data;
param  r: 1 2 3 4 :=
        1   0 1 0 0
        2   1 0 1 0
        3   1 1 0 0
        4   0 1 1 1
;
#*/


#option presolve 0;
#option cplex_options "sensitivity";
# option solver cplex;
# option solver bonmin;
option solver cbc;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
# option solver LaGO;
# option solver loqo;
#option lpsolve_options "wlp=xxx.lp";
#option solver lpsolve;
#option solver minos;
#option solver pcx;
# option solver snopt;
#option solver umsip;

solve;

display r;
display d;
display x;

for{i in N} {
   for{j in N} {
      printf "%d ",x[i,j];
   }
   printf "\n";
}


/*
data;
param  r: 1 2 3 4 :=
        1   0 1 0 0
        2   1 0 1 0
        3   1 1 0 0
        4   0 1 1 1
;
*/

end;



