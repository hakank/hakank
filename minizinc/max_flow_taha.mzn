%
% Max flow problem in Minizinc.
% 
% From http://taha.ineg.uark.edu/maxflo.txt
% Taha "Introduction to Operations Research", Example 6.4-2)
% (ported from the AMPL code)
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int:  n;   
int:  start;
int:  end;
array[1..n, 1..n] of int: c;

array[1..n,1..n] of var int: x;
array[1..n] of var int: outFlow;
array[1..n] of var int: inFlow;

var int: total= sum(j in 1..n where c[start,j]>0) (x[start,j]);

solve maximize total;
% solve satisfy;

constraint  
%  total >= 60
%  /\
  forall(i,j in 1..n) (
    x[i,j] >= 0 /\ x[i,j] <= c[i,j]
  )
  /\
  forall(i in 1..n) (
    inFlow[i] =sum(j in 1..n where c[j,i]>0) (x[j,i])
  )
  /\
  forall(i in 1..n) (
     outFlow[i] = sum(j in 1..n where c[i,j]>0) (x[i,j])
  )
  /\
  forall(i in 1..n where i != start /\ i != end) (
        outFlow[i]-inFlow[i]=0
  )
  /\
  sum(i in 1..n where c[i,start]>0) (x[i,start])=0 
  /\
  sum(j in 1..n where c[end,j]>0 ) (x[end,j]) = 0
;

%
% data;
%
n     = 5;
start = 1;
end   = 5;
c = array2d(1..n, 1..n,
[
 0, 20, 30, 10,  0,
 0,  0, 40,  0, 30,
 0,  0,  0, 10, 20,
 0,  0,  5,  0, 20,
 0,  0,  0,  0,  0
]);


output [
  if i = 1 /\ j = 1 then
   "\ntotal: " ++ show(total) 
  else "" endif ++
  if j = 1 then "\n" else " " endif ++
  show_int(2,x[i,j])
  | i, j in 1..n
] ++ ["\n"];

