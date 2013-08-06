% 
% Enigma puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 7
% Description  : Enigma
% Source       : Herald Tribune circa November 1979 - courtesy of Dr Tito A. Ciriani

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s7.html

% This is an integer programming model.
% Cf a constraint programming model in enigma2.mzn .
%

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of 1..10: N = 1..10;

% x(i,j) = 1 if digit (i-1) represents letter j
% letter code 1 = O, 2 = N, 3 = E, 4 = T, 5 = W
%             6 = H, 7 = R, 8 = L, 9 = V,10 = Y
% example: x(10,10) = 1 means digit 9 is assigned to letter Y
array[N, N] of var 0..1: x;

% for presentation (not in Chlond's model)
array[1..10] of var 0..9: y;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in N], first_fail, indomain_min, complete) satisfy;

constraint  
  % for the presentation array y (not in 
  forall(i in N) ( 
    let {
      var 1..10: j
    } 
    in
    x[i,j] = 1
    /\
    y[i] = j-1
  )
  /\
  forall(i in N) (
        sum(j in N) (x[i,j]) = 1  % each digit assigned once
  )
  /\
  forall(j in N) (
    sum(i in N) (x[i,j]) = 1  % each letter assigned one digit
  ) 
  /\ % and the "equations" (note: this is an integer programming model)
  sum(i in N) (100*(i-1)*x[i,1]) + sum(i in N) (10*(i-1)*x[i,2]) + sum(i in N) ((i-1)*x[i,3]) +
  sum(i in N) (100*(i-1)*x[i,1]) + sum(i in N) (10*(i-1)*x[i,2]) + sum(i in N) ((i-1)*x[i,3]) +
  sum(i in N) (100*(i-1)*x[i,4]) + sum(i in N) (10*(i-1)*x[i,5]) + sum(i in N) ((i-1)*x[i,1]) +
  sum(i in N) (100*(i-1)*x[i,4]) + sum(i in N) (10*(i-1)*x[i,5]) + sum(i in N) ((i-1)*x[i,1]) +
  sum(i in N) (10000*(i-1)*x[i,4]) + sum(i in N) (1000*(i-1)*x[i,6]) + sum(i in N) (100*(i-1)*x[i,7]) +
  sum(i in N) (10*(i-1)*x[i,3]) + sum(i in N) ((i-1)*x[i,3]) +
  sum(i in N) (100000*(i-1)*x[i,3]) + sum(i in N) (10000*(i-1)*x[i,8]) + sum(i in N) (1000*(i-1)*x[i,3]) +
  sum(i in N) (100*(i-1)*x[i,9]) + sum(i in N) (10*(i-1)*x[i,3]) + sum(i in N) ((i-1)*x[i,2]) = 

  sum(i in N) (100000*(i-1)*x[i,4]) + sum(i in N) (10000*(i-1)*x[i,5]) + sum(i in N) (1000*(i-1)*x[i,3]) + 
  sum(i in N) (100*(i-1)*x[i,2]) + sum(i in N) (10*(i-1)*x[i,4]) + sum(i in N) ((i-1)*x[i,10])
;

output  
[ 
  "y: ", show(y) ++ "\n"
]
;
