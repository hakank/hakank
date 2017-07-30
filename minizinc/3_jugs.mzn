% Three jugs problem in Minzinc modelled as a shortest path problem.
%
% Problem from Taha "Introduction to Operations Research", page 245f
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc



int: n = 15;
int: start = 1; % start node
int: end = 15;  % end node
int: M = 999; % large number

array[1..n, 1..n] of 0..M: d; % distance
array[1..n] of var -1..1: rhs; % requirements (right hand statement)

% objective to minimize
var int: z = sum(i in 1..n, j in 1..n where d[i,j]<M) ( d[i,j]*x[i,j]);

array[1..n, 1..n] of var 0..1: x; % the resulting matrix, 1 if connected, 0 else
array[1..n] of var 0..1: outFlow;
array[1..n] of var 0..1: inFlow;

% solve minimize z;
solve :: int_search(inFlow ++ outFlow ++ rhs ++ [ x[i,j] | i,j in 1..n], first_fail, indomain_split, complete) minimize z;


constraint 
  % start or end ?
  forall(i in 1..n) ( 
    if i = start then 
      rhs[i] = 1
    elseif i = end then 
      rhs[i] = -1
    else  
      rhs[i] = 0
    endif
  )
  /\ % outflow constraint
  forall(i in 1..n) (
    outFlow[i] = sum(j in 1..n where d[i,j]<M) (x[i,j])
  )
  /\ % inflow constraint
  forall(j in 1..n) (
    inFlow[j]  = sum(i in 1..n where d[i,j]<M) (x[i,j])
  )
  /\ % inflow = outflow
  forall(i in 1..n) (outFlow[i]-inFlow[i]=rhs[i])

; 


% data
d =
array2d(1..n, 1..n,
[
  M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M,
  M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M,
  M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M,
  M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M,
  M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M,
  M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M,
  M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M,
  M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
  M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M,
  M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M,
  M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M,
  M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M,
  M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M,
  M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
  M, M, M, M, M, M, M, M, M, M, M, M, M, M, M
])
;

% shows the result matrix
output [
       if i = 1 /\ j = 1 then 
         "z      : " ++ show(z) ++ "\n" ++
         "rhs    : " ++ show(rhs) ++ "\n" ++ 
         "inFlow : " ++ show(inFlow) ++ "\n" ++ 
         "outFlow: " ++ show(outFlow) ++ "\n" ++
         "     1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n" 
       else "" endif ++
       if j = 1 then show_int(2,i) ++ " : " else "" endif ++
       show(x[i,j]) ++ if j = n then "\n" else " " endif
       | i in 1..n, j in 1..n
]

++ ["\norder:\n"] ++
[

 show("x")

]
;
