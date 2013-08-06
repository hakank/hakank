%
% A Minizinc implementation of "Devil's Word"
%
% i.e. addition/subtraction of an array of numbers to give a specific total (e.g. 666)
%
% Compare to my CGI program "Devil's Word"
%  http://www.hakank.org/data_snooping/666.cgi
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: n; % size of the number array
var int: total; % the sum (if unset: generates all solutions) 
array[1..n] of int: arr; % the numbers
array[1..n] of var 0..1: plus;  % is the number arr[i] to be added
array[1..n] of var 0..1: minus; % or is it to be subtracted

% array with the number with correct sign
array[1..n] of var -max(arr)..max(arr): result; 

 % number of minus entries (maybe to be minimized)
var int: num_minus = sum(i in 1..n) (bool2int(minus[i] = 1));

constraint 
          % calculate the sum of the numbers in arr
          total = sum(i in index_set(arr)) (arr[i]*plus[i] + (-arr[i])*minus[i])
          % total = sum(i in 1..n) (result[i]) % alternative summation
          /\ 
          forall(i in 1..n) (
              % just one of plus and minus
              (plus[i] + minus[i] = 1) 
              /\ % calculate the result array
              result[i] = arr[i]*plus[i] + (-arr[i])*minus[i]
          )
;
 
% Minimize the number of negative numbers
% solve minimize num_minus;
solve satisfy;
% solve :: int_search(result, "first_fail", "indomain", "complete") satisfy;

%
% data
%

%
% Test data
%
% n = 100;
% total = 666;
% total = 10;
%                    
% arr = [1,2,3,4,5,6,7,8,9,10];
% arr = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25];
% arr = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100];


% My name ("Håkan Kjellerstrand") in ASCII numbers.
% Cf http://www.hakank.org/data_snooping/666.cgi?name=H%E5kan+Kjellerstrand&submit=ok
% which gives the solution:
% +72+229+107+97+110+32+75-106+101+108-108+101-114-115-116-114+97+110+100 = 666
%
% There are 288 different solutions...
%
n = 19;
total = 666;
arr = [72, 229, 107, 97, 110, 32, 75, 106, 101, 108, 108, 101, 114, 115, 116, 114, 97, 110, 100];


%
% Output
%
output [
     "total:  ", show(total),  "\n",
     "result: ", show(result), "\n",
     "plus:   ", show(plus),   "\n",
     "minus:  ", show(minus),  "\n",

];

