/*
# Stanley, Alric and Leonard can do a job in 16, 25 and 14 months
respectively.
# Stanley started the work. After he worked for 3 months, Alric
joined him.
# They worked together for another 3 months, then Leonard joined
them. How
# many months they worked to complete the job?

Solution:
========
months = 2.90349


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

var months;

minimize time: months;

subject to c1: (months+6)/16 + (months+3)/25 + months/14 = 1;

solve;
display months;

end;
