/*
  Thu Jan  3 22:43:23 2008/hakank@bonetmail.com

  A third version of least_diff which is inspired by
  send_more_money3.mod


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set ints := {"a","b","c","d","e","f","g","h","i","j"};
set digits := 0..9;
var y{ints} >= 0  integer;
var x{ints, digits} binary;

minimize z: 
        10000*y['a'] + 1000*y['b'] + 100*y['c'] + 10*y['d'] + y['e'] -
       (10000*y['f'] + 1000*y['g'] + 100*y['h'] + 10*y['i'] + y['j'])
;

subject to problem:
         10000*y['a'] + 1000*y['b'] + 100*y['c'] + 10*y['d'] + y['e'] -
        (10000*y['f'] + 1000*y['g'] + 100*y['h'] + 10*y['i'] + y['j']) >= 0
;

# ydef(i).. y(i) =e= sum(j, x(i,j)*v(j)))))))
# subject to ydef{i in ints}: y[i] = sum{j in digits} x[i,j]*v[j];
subject to ydef{i in ints}: y[i] = sum{j in digits} x[i,j]*j;

# xrow(i).. sum(j,x(i,j)) =e= 1)))
# unique row
subject to xrow{i in ints}: sum{j in digits} x[i,j] = 1;
# xcol(j).. sum(i,x(i,j)) =e= 1)))
# unique column
subject to xcol{j in digits}: sum{i in ints} x[i,j] = 1;


#subject to test1:
#        y['a'] >= 1;



#option presolve 0;
#option cplex_options "sensitivity";
#option solver cplex;
#option bonmin_options 'bonmin.algorithm B-BB'; # simple branch-and-bound algorithm,
# option bonmin_options 'bonmin.algorithm B-OA'; # OA Decomposition algorithm,
#option bonmin_options 'bonmin.algorithm B-QG'; # Quesada and Grossmann branch-and-cut algorithm,
# option bonmin_options 'bonmin.algorithm B-Hyb'; # hybrid outer approximation based branch-and-cut,
#option solver bonmin;
option solver cbc;
# option solver donlp2;
# option solver gjh;
# option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#  option solver LaGO;
# option solver loqo;
#  option solver lpsolve;
# option solver minos;
# option solver pcx;
# option solver snopt;
# option solver umsip;

solve;

display y;

display "Solution should be [50123-49876=247]";
display "abcde: ", 10000*y['a'] + 1000*y['b'] + 100*y['c'] + 10*y['d'] + y['e'];
display "fghij: ", (10000*y['f'] + 1000*y['g'] + 100*y['h'] + 10*y['i'] + y['j']);
display 10000*y['a'] + 1000*y['b'] + 100*y['c'] + 10*y['d'] + y['e'] - (10000*y['f'] + 1000*y['g'] + 100*y['h'] + 10*y['i'] + y['j']);

display z;

end;


