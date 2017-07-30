% 
% Euler problem 39 in MiniZinc.
% 
% """
% If p is the perimeter of a right angle triangle with integral length 
% sides, {a,b,c}, there are exactly three solutions for p = 120.
% 
% {20,48,52}, {24,45,51}, {30,40,50}
% 
% For which value of p <= 1000, is the number of solutions maximised?
% 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: n_sols = 10;
array[1..n_sols, 1..4] of var 0..1000: s;

var 1..n_sols: num_solutions = sum(i in 1..n_sols) (bool2int(s[i,1] > 0));

solve maximize num_solutions;
% solve satisfy;
% solve :: int_search(
%         [s[i,j] | i in 1..n_sols, j in 1..4], %  ++ [num_solutions], 
%         first_fail, indomain_split, complete) 
%         maximize num_solutions;
%         % satisfy;

constraint

  s[1,4] <= 1000
  /\
  forall(i in 1..n_sols) (

    (   
     s[i,1] > 0 /\ s[i,2] > 0 /\ s[i,3] > 0 /\
     s[i,1] < s[i,4] /\ s[i,1] < s[i,4] /\ s[i,1] < s[i,4] /\
     s[i,1] <= s[i,2] /\ s[i,2] <= s[i,3] /\
     s[i,3]*s[i,3] = s[i,1]*s[i,1] + s[i,2]*s[i,2] /\
     s[i,1] + s[i,2] + s[i,3] = s[i,4]
    )
    \/
    (
       s[i,1] = 0 /\ s[i,2] = 0 /\ s[i,3] = 0 /\ s[i,4] = 0
    )
  )
   /\
   forall(i in 2..n_sols) (
       (s[i,1] < s[i-1,1] \/
        s[i,1] = 0
       )
       /\
       (
         s[i,4] = s[i-1,4] \/ 
         s[i,4] = 0
       )
       /\
       forall(j in 1..4) (
         s[i-1,j] = 0 ->  s[i,j] = 0
       )
   )
;


output 
[  
  "\nnum_solutions: ", show(num_solutions)
] ++
[
  
  if j = 1 then "\n" else " " endif ++
    show(s[i,j])
  | i in 1..n_sols, j in 1..4
]
++
[
  "max p: " ++ show(s[1,4])
]
;