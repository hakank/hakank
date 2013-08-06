/*
Nolan, Irving and Olive can do a job in 15, 24 and 16 hours
respectively.
Nolan worked at it for 6 hours and Irving worked at it for 4 hours.
Olive
finished the remaining job. How many hours Olive require to
complete the  remaining job?

Solution:
========
hour = 6.93333


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/
var hour;

minimize time: hour;

subject to c1: 6/15 + 4/24 + hour/16 = 1;

solve;
display hour;


end;

