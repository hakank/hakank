% 
% Arrow problem in MiniZinc.
% 
% From the Swedish blog post "Pilen" (Arrow)
% http://mattebloggen.com/2012/03/4459/
% (translated):
% "The digits 1 to nine is filling the matrix as seen in the
% left picture. You can walk on the cells on the grid but
% never go back to a visited cell. You must always go to a 
% neighboring cell.
%
%   1 8 4            
%   6 3 9
%   5 7 2
%
% Katarina walked along the arrow shown on the right picture.
% She wrote down the digits in order and got the number 84937561. 
% Your assignment is to draw another way which give a larger number,
% the larger the better.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 3;
array[1..n, 1..n] of int: data;


% decision variable
array[1..n*n] of var 1..n*n: x;
var 123456789..987654321: num;

predicate toNum(array[int] of var int: a, var int: n) =
    let { 
       int: len = length(a) 
    } in
    n = sum(i in index_set(x)) ( pow(10, len-i)*a[i] )
;

% solve satisfy;
solve :: int_search(x, 
        first_fail, 
        indomain_max, 
        complete) 
 maximize num;

constraint 
  alldifferent(x) /\
  toNum(x, num)
;

constraint
  forall(k in 1..n*n-1) (
     let {
        var 1..n: i,
        var 1..n: j,
        var {-1,0,1}: a,
        var {-1,0,1}: b
     }
     in
       % find this number
       x[k] = data[i,j]

       /\ % find the possible offsets for the next number   
       i+a >= 1 /\ j+b >=  1 /\
       i+a <= n /\ j+b <= n  /\ 
       (a = 0 \/ b = 0)

       /\ 
       % and connect to the next number
       x[k+1] = data[i+a, j+b]  
  )
  /\
  num > 84937561 % Larger than Katarina's number

;

output [
  "num: " ++ show(num) ++ "\n" ++
  "x: " ++ show(x)
]
 ++ ["\n"]
;


%
% Data
%
data = array2d(1..n, 1..n,
 [
  1, 8, 4,            
  6, 3, 9,
  5, 7, 2,
 ]);