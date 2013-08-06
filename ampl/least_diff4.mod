/*
  Thu Jan  3 22:43:23 2008/hakank@bonetmail.com

  Fourth version of least_diff, inspired by
  send_more_money4.mod


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set letters := {"a","b","c","d","e","f","g","h","i","j"};
set digits := 0..9;
# var y{ints} >= 0  integer;
# var x{ints, digits} binary;
var x{letters} >= 0 <= 9    integer;
var y{letters, letters} binary; # for all_different
param M := 10;

minimize z: 
        10000*x['a'] + 1000*x['b'] + 100*x['c'] + 10*x['d'] + x['e'] -
       (10000*x['f'] + 1000*x['g'] + 100*x['h'] + 10*x['i'] + x['j'])
;

subject to problem:
         10000*x['a'] + 1000*x['b'] + 100*x['c'] + 10*x['d'] + x['e'] -
        (10000*x['f'] + 1000*x['g'] + 100*x['h'] + 10*x['i'] + x['j']) >= 0
;

#
# all different, see magic_square_taha.mod for more about this representation
#
all_different1{i in letters, j in letters}: 
   M*y[i,j]       + (x[i] - x[j]) >= (if i <> j then 1);

all_different2{i in letters, j in letters}: 
   M*(1 - y[i,j]) + (x[j] - x[i]) >= (if i <> j then 1);


option solver cplex;

solve;

display x;

display "Solution should be [50123-49876=247]";
display "abcde: ", 10000*x['a'] + 1000*x['b'] + 100*x['c'] + 10*x['d'] + x['e'];
display "fghij: ", (10000*x['f'] + 1000*x['g'] + 100*x['h'] + 10*x['i'] + x['j']);
display 10000*x['a'] + 1000*x['b'] + 100*x['c'] + 10*x['d'] + x['e'] - (10000*x['f'] + 1000*x['g'] + 100*x['h'] + 10*x['i'] + x['j']);

display z;

end;


