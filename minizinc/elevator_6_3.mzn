%
% Data for 
% Martin Chlond "A Tokyo Elevator Puzzle"
% http://ite.pubs.informs.org/Vol6No3/Chlond/
% 
% Data from http://ite.pubs.informs.org/Vol6No3/Chlond/data1.php
%

include "elevator_model.mzn";

%
% Data set for elevator puzzle
% 6 floors, 3 visits (excluding top and bottom)
% 
m = 20;
n = 6;

c = array2d(1..m, 1..n,
[
1, 1, 1, 0, 0, 0, 
1, 1, 0, 1, 0, 0, 
1, 1, 0, 0, 1, 0, 
1, 1, 0, 0, 0, 1, 
1, 0, 1, 1, 0, 0, 
1, 0, 1, 0, 1, 0, 
1, 0, 1, 0, 0, 1, 
1, 0, 0, 1, 1, 0, 
1, 0, 0, 1, 0, 1, 
1, 0, 0, 0, 1, 1, 
0, 1, 1, 1, 0, 0, 
0, 1, 1, 0, 1, 0, 
0, 1, 1, 0, 0, 1, 
0, 1, 0, 1, 1, 0, 
0, 1, 0, 1, 0, 1, 
0, 1, 0, 0, 1, 1, 
0, 0, 1, 1, 1, 0, 
0, 0, 1, 1, 0, 1, 
0, 0, 1, 0, 1, 1,
0, 0, 0, 1, 1, 1 
])
;

% output [
%  if i = 1 then 
%    "summa: " ++ show(summa) ++ "\n"
%  else "" endif ++
%  show(i) ++ ": " ++ show(x[i]) ++ "\n"
%  | i in 1..m
% ];