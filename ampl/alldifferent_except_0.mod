/*

  Alldifferent except 0 in AMPL+CP.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

set dom := 1..n;
var x{dom} >= 0 <= n integer;

minimize obj: x[1];

# Alldifferent except 0
s.t. alldifferent_except0{i in dom,j in dom: i < j}: 
        (x[i] > 0 && x[j] > 0) ==> (x[i] != x[j]);

#
# And add constraint about the number of 0s
#
# s.t. zeros: count{i in dom} (x[i] = 0) >= 1;
# s.t. zeros2: count{i in dom} (x[i] = 0) <= 3;
s.t. zeros3: exactly 2 {i in dom} (x[i] = 0);

data;

let n := 301;

option solver gecode;

solve;

# display dom;
display x;
