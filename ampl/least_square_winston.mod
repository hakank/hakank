/*
  Thu Jan  3 22:28:15 2008/hakank@bonetmail.com

  Winston OR, least square.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

var a;
var b;
param len >= 0;
param x{1..len, 1..2};

minimize z:
        #                 ( y - a - b*x)^2
        sum{i in 1..len} (x[i,2] - a - b * x[i,1])^2;

data;

/*
param len := 7;
param x: 1 2 :=
        1 1 2
        2 2 4
        3 3 6
        4 4 8
        5 5 10
        6 6 12
        7 7 14
;
*/

# From http://standards.nctm.org/document/eexamples/chap7/7.4/index.htm
# R exempel
# year <- c(2000 ,   2001  ,  2002  ,  2003 ,   2004)
# rate <- c(9.34 ,   8.50  ,  7.62  ,  6.93  ,  6.60)))
# Answer: 1419.208       -0.705  
param len := 5;
param x: 1 2 :=
        1 2000 9.34
        2 2001 8.50
        3 2002 7.62
        4 2003 6.93
        5 2004 6.60
;      

#option presolve 0;
#option cplex_options "sensitivity";
#option solver cplex;
#option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display z, a, b;
