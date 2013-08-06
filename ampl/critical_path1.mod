/*
  Tue Jan  1 11:16:34 2008/hakank@bonetmail.com

  Winston OR, page 441 Critical Path

  The Lingo model Widget1.lng gives the following solution:

Variable           Value        Reduced Cost
TIME( 1)        0.000000            0.000000
TIME( 2)        9.000000            0.000000
TIME( 3)        9.000000            0.000000
TIME( 4)        16.00000            0.000000
TIME( 5)        26.00000            0.000000
TIME( 6)        38.00000            0.000000

And cplex give the same:

TIME [*] :=
1   0
2   9
3   9
4  16
5  26
6  38
;


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set NODES;
set ARCS within {NODES cross NODES};
param DUR{ARCS};
var TIME{NODES} >= 0;

# last - first
minimize z:
        TIME[6]-TIME[1];

# @FOR(ARCS(I,J):TIME(J)>TIME(I)+DUR(I,J));
subject to c1{ (i,j) in ARCS}:
        TIME[j] >= TIME[i] + DUR[i,j];
        



data;

set NODES := 1 2 3 4 5 6;
param: ARCS: DUR :=
       1,2  9,
       1,3  6,
       2,3  0,
       3,4  7,
       3,5  8,
       4,5  10,
       5,6  12
;




#option cplex_option "sensitivity";
#option solver cplex;
# option solver bonmin;
solve;
#display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;

display z;
display TIME;
