/*
  Thu Jan  3 22:43:23 2008/hakank@bonetmail.com

  A variant of magic square inspired by send_more_money3.mod

10 +  8 + 15 +  7 
14 + 11 +  6 +  1 
 4 + 13 + 12 + 16 
 2 +  5 +  3 +  9


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/

*/

param n; # (i.e. n^2 numbers)
var s; # the sum

# param v{0..9} >= 0 integer;
set digits := 1..n^2;
# v(i) = ord(i)-1; 
# param v{digits} integer;
var y{digits} >= 0  integer;
var x{digits, digits} binary;

maximize z: 
        s
#        sum{j in digits} x[1,j]
;

subject to rows{i in digits}:
        sum{j in digits} x[i,j] = s
;

subject to cols{j in digits}:
        sum{i in digits} x[i,j] = s
;



# ydef(i).. y(i) =e= sum(j, x(i,j)*v(j)))))))
# subject to ydef{i in ints}: y[i] = sum{j in digits} x[i,j]*v[j];
subject to ydef{i in digits}: y[i] = sum{j in digits} x[i,j]*j;

# xrow(i).. sum(j,x(i,j)) =e= 1)))
# row
subject to xrow{i in digits}: sum{j in digits} x[i,j] = 1;
# xcol(j).. sum(i,x(i,j)) =e= 1)))
# column
subject to xcol{j in digits}: sum{i in digits} x[i,j] = 1;


#subject to test1:
#        y['a'] >= 1;

data;
param n:=4;


#option presolve 0;
#option cplex_options "sensitivity";
option solver cplex;
# option bonmin_option 'bonmin.algorithm B-BB'; # simple branch-and-bound algorithm,
# option bonmin_option 'bonmin.algorithm B-OA'; # OA Decomposition algorithm,
# option bonmin_option 'bonmin.algorithm B-QG'; # Quesada and Grossmann branch-and-cut algorithm,
#  option bonmin_option 'bonmin.algorithm B-Hyb'; # hybrid outer approximation based branch-and-cut,
#  option bonmin_option 'bonmin.algorithm B-Ecp'; # ecp cuts based branch-and-cut a la FilMINT.
#option solver bonmin;
#option solver cbc;
# option solver donlp2;
# option solver gjh;
# option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver loqo;
#option solver lpsolve;
# option solver minos;
# option solver pcx;
# option solver snopt;
# option solver umsip;

solve;

display z;
display x;
display y;
display s;

#for{i in 1..n^2} {
#  for{j in 1..n^2: x[i,j] > 0.1} {
#     printf "%d ", j;
#  }
#}
#printf "\n";

#data;
#param n:=4;

end;


