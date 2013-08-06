/*

  Olympic puzzle in AMPL.

  Benchmark for Prolog (BProlog)
  """
  File   : olympic.pl
  Author : Neng-Fa ZHOU
  Date   : 1993

  Purpose: solve a puzzle taken from Olympic Arithmetic Contest.

  Given ten variables with the following configuration:

                 X7   X8   X9   X10
                    X4   X5   X6
                       X2   X1             
                          X1

  We already know that X1 is equal to 3 and want to assign each variable
  with a different integer from {1,2,...,10} such that for any three
  variables 
                        Xi   Xj

                           Xk
  the following constraint is satisfied:

                      |Xi-Xj| = Xk
  """
  


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;


var x{1..n} >= 1 <= n integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];

s.t. c2: x[1] = 3;

s.t. c3:
    x[1] = abs(x[2]-x[3]) and
    x[2] = abs(x[4]-x[5]) and
    x[3] = abs(x[5]-x[6]) and
    x[4] = abs(x[7]-x[8]) and
    x[5] = abs(x[8]-x[9]) and
    x[6] = abs(x[9]-x[10])
;

data;

param n := 10;


option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;


display x;

