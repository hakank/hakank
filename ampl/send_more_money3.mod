/*
  Thu Jan  3 22:43:23 2008/hakank@bonetmail.com

  A third version of SEND+MORE=MONEY based on the GAMS example
    http://www.gams.com/~erwin/recr/alphametics.pdf
     http://www.gams.com/modlib/adddocs/alphametdoc.htm
  which don't use the sum hack as in send_more_money2.mod

The following solver manage to solve it:
  cplex (snabbt, 0.03s)
  bonmin (snabbt med rätt option, 0.22s)
  cbc (hyfsat snabbt, 0.44s)
  lpsolve (snabbt, 0.04s)

With 
 option bonmin_options 'bonmin.algorithm B-QG';
bonmin solves it fast (0.22s),

The formal structure of this:

  set ints;
  set digits := 0..9;
  var y{ints} >= 0 integer;   # the assignment of digits
  var x{ints, digits} binary; # matrix: integer and binary
  # may not even have an objective, just "find a feasible solution"
  minimize z: 0; 

  subject to problem:
          # the problem
          ;

  # for each entry in y: set the appropriate digit (from x)
  subject to ydef{i in ints}: y[i] = sum{j in digits} x[i,j]*j;

  # must be only one per row/column
  subject to xrow{i in ints}: sum{j in digits} x[i,j] = 1;
  subject to xcol{j in digits}: sum{i in ints} x[i,j] = 1;

  # and maybe auxilliary constraints


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

set ints := {"s","e","n","d","m","o","r","y","dummy1","dummy2"};

set digits := 0..9;

var y{ints} >= 0  integer;
var x{ints, digits} binary;

# dummy_objective
minimize z: 
        0
;

subject to problem:
                  1000*y['s'] + 100*y['e'] + 10*y['n'] + y['d'] + 
                  1000*y['m'] + 100*y['o'] + 10*y['r'] + y['e'] =
   10000*y['m'] + 1000*y['o'] + 100*y['n'] + 10*y['e'] + y['y']
;

# ydef(i).. y(i) =e= sum(j, x(i,j)*v(j)))))))
#subject to ydef{i in ints}: y[i] = sum{j in digits} x[i,j]*v[j];
subject to ydef{i in ints}: y[i] = sum{j in digits} x[i,j]*j;

# xrow(i).. sum(j,x(i,j)) =e= 1)))
subject to xrow{i in ints}: sum{j in digits} x[i,j] = 1;
# xcol(j).. sum(i,x(i,j)) =e= 1)))
subject to xcol{j in digits}: sum{i in ints} x[i,j] = 1;

# bounds on leading digits, they can not be 0
# specialare för smm
subject to s_not_zero: y['s'] >= 1;
subject to m_not_zero: y['m'] >= 1;

#option auxfiles rc;

#option presolve 0;
#option cplex_options "sensitivity";
#option cplex_options 'writeprob=send_more_money3.lp';
option solver cplex;
#option bonmin_options 'bonmin.algorithm B-BB'; # simple branch-and-bound algorithm,
#option bonmin_options 'bonmin.algorithm B-OA'; # OA Decomposition algorithm,
#option bonmin_options 'bonmin.algorithm B-Hyb'; # hybrid outer approximation based branch-and-cut,
#option bonmin_options 'bonmin.algorithm B-QG'; # Quesada and Grossmann branch-and-cut algorithm, # Denna ska man använda till SMM.
#option solver bonmin;
#option solver cbc;
# option solver donlp2;
# option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=SNOPT';
#option solver kestrel;
#option solver LaGO;
#option solver loqo;
#option solver lpsolve;
# option solver minos;
# option solver pcx;
# option solver snopt;
# option solver umsip;
#option solver pswarm;

solve;

display y;

printf "Should be: S:9 E:5 N:6 D:7 M:1 O:0 R:8 Y:2\n";
printf "           S:%d E:%d N:%d D:%d M:%d O:%d R:%d Y:%d \n", y["s"], y["e"],y["n"],y["d"],y["m"],y["o"],y["r"],y["y"] ;

display "SEND: ",                 1000*y["s"] + 100*y["e"] + 10*y["n"] + y["d"];
display "MORE: ",                 1000*y["m"] + 100*y["o"] + 10*y["r"] + y["e"];
display "SEND+MORE: ", 1000*y["s"] + 100*y["e"] + 10*y["n"] + y["d"] + 1000*y["m"] + 100*y["o"] + 10*y["r"] + y["e"]; 
display "MONEY: ", 10000*y["m"] + 1000*y["o"] + 100*y["n"] + 10*y["e"] + y["y"];
display               1000*y["s"] + 100*y["e"] + 10*y["n"] + y["d"] + 
                      1000*y["m"] + 100*y["o"] + 10*y["r"] + y["e"] - 
        (10000*y["m"] + 1000*y["o"] + 100*y["n"] + 10*y["e"] + y["y"]);


write msend_more_money3;

end;


