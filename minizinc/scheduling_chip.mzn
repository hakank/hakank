% 
% Scheduling problem in MiniZinc.
% 
% This problem is from 
% Nicolas Beldiceanu & Evelyne Contejean 
% "Introducing Global Constraints in CHIP"
% page 3f
% 
% """
% The aim is to find a schedule that minimises the general end while not 
% exceeding the capacity 13 of the resource.
% 
% task       t1 t2 t3 t4 t5 t6 t7
% duration   16  6 13  7  5 18  4    
% resource    2  9  3  7 10  1 11
%
% ....
%
% optimal solution of cost 23: [1,17,10,10,5,5,1]
%
% """"
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


%
% Note: there are two solution of cost 23 (with solve satisfy)
% End      : 23
% origin   : [1, 17, 10, 10, 5, 5, 1]
% duration : [16, 6, 13, 7, 5, 18, 4]
% resources: [2, 9, 3, 7, 10, 1, 11]
% end time : [17, 23, 23, 17, 10, 23, 5]
% 
% End      : 23
% origin   : [7, 1, 1, 7, 14, 1, 19]
% duration : [16, 6, 13, 7, 5, 18, 4]
% resources: [2, 9, 3, 7, 10, 1, 11]
% end time : [23, 7, 14, 14, 19, 19, 23]
%

include "globals.mzn"; 

int: n = 7;
int: m = 30;

int: Limit; 
array[1..n] of 1..m: LD; % duration
array[1..n] of 1..m: LR; % resources

% decision variables
var int: End; % to minimize
array[1..n] of var 1..m: LO; % origin (start)
array[1..n] of var 1..m: LE; % end times


% solve satisfy;
% solve minimize End;
solve :: int_search(LE, 
        anti_first_fail, indomain_min, complete) minimize End;

constraint
  % End <= 23 % for solve satisfy
  % /\
  forall(i in 1..n) (
      LO[i] + LD[i] = LE[i] 
  )
  /\
  maximum(End, LE)
  /\
  cumulative(LO, LD, LR, Limit)

;


output [
  "Max end time: ", show(End), "\n",
  "Origin      : ", show(LO), "\n",
  "Duration    : ", show(LD), "\n",
  "Resources   : ", show(LR), "\n",
  "End times   : ", show(LE), "\n",

];

%
% data
%

LD = [16, 6,13, 7, 5,18, 4];
LR = [ 2, 9, 3, 7,10, 1,11];
Limit = 13;
