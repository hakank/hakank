% 
% Logical design problem in MiniZinc.
% 
% From H. Paul Williams "Model Building in Mathematical Programming", 4th edition
% The Logical design problem, sections 12.12, 13.12 and 14.12.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


set of int: maxi = 1..7;
set of int: maxii = 1..3;
set of int: maxj = 1..2;
set of int: maxk = 1..4;

array[maxk,maxj] of 0..1: alpha = array2d(maxk, maxj, [0,0,0,1,1,0,1,1]);

array[maxi,maxj] of var 0..1: in1;
array[maxi,maxk] of var 0..1: out;
  	
array[maxi] of var 0..1: nor;

var int: number = sum(i in maxi) (nor[i]);
solve minimize number;
% solve satisfy; % four solutions

constraint
%        number =  5
%        /\
	forall(i in maxi, j in maxj) (nor[i]-in1[i,j] >= 0) /\ 
        forall(i in 1..1) (nor[2]+nor[3] + sum(j in maxj) (in1[i,j]) <= 2) /\ 
	forall(i in 2..2) (nor[4]+nor[5] + sum(j in maxj) (in1[i,j]) <= 2) /\ 
	forall(i in 3..3) (nor[6]+nor[7] + sum(j in maxj) (in1[i,j]) <= 2) /\ 
	forall(k in maxk, i in maxii) (out[2*i,k]+out[i,k] <= 1) /\ 
	forall(k in maxk, i in 1..1) (out[3,k]+out[i,k] <= 1) /\ 
	forall(k in maxk, i in 2..2) (out[5,k]+out[i,k] <= 1) /\ 
	forall(k in maxk, i in 3..3) (out[7,k]+out[i,k] <= 1) /\ 
	forall(k in maxk, i in maxi, j in maxj) (alpha[k,j]*in1[i,j]+out[i,k] <= 1) /\ 
	forall(k in maxk, i in 1..1) ( 
             out[2,k]+out[3,k]+sum(j in maxj) (alpha[k,j]*in1[i,j])+out[i,k]-nor[i] >= 0
         
        )
        /\ 
        forall(k in maxk, i in 2..2) ( 
                   out[4,k]+out[5,k]+sum(j in maxj) (
                      alpha[k,j]*in1[i,j])+out[i,k]-nor[i] >= 0
        )
        /\
        forall(k in maxk, i in 3..3) ( 
                   out[6,k]+out[7,k]+sum(j in maxj) (
                      alpha[k,j]*in1[i,j])+out[i,k]-nor[i] >= 0
        )
        /\ 
        forall(k in maxk, i in 4..7) ( 
                   sum(j in maxj) (alpha[k,j]*in1[i,j])+(out[i,k]-nor[i]) >= 0
        ) 
        /\ 
	forall(i in maxi, k in maxk) (nor[i]-out[i,k] >= 0) /\ 
	forall(i in maxi) ( nor[i] <= 1) /\ 
        forall(i in maxi, j in maxj) (in1[i,j] <= 1) /\ 
        forall(i in 2..7, k in maxk) (out[i,k] <= 1) /\ 
        forall(i in 1..1, k in 1..1) (out[i,k] = 0) /\ 
        forall(i in 1..1, k in 2..2) (out[i,k] = 1) /\ 
        forall(i in 1..1, k in 3..3) (out[i,k] = 1) /\ 
        forall(i in 1..1, k in 4..4) (out[i,k] = 0) /\
        nor[1] >= 1
;

output 
["\nnumber: ", show(number)] ++
["\nnor: ", show(nor)] ++
[ "\nin1:"] ++
[
  if j = 1 then "\n" else " " endif ++
    show(in1[i,j])
  | i in maxi, j in maxj
] ++
[ "\nout:"] ++
[
  if k = 1 then "\n" else " " endif ++
    show(out[i,k])
  | i in maxi, k in maxk
] ++ ["\n"]
;