% 
% Decentralization problem in MiniZinc.
% 
% From H. Paul Williams "Model Building in Mathematical Programming", 4th edition
% Decentralization, sections 12.10, 13.10 and 14.10.
%


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of int: ndepts = 1..5;  % departments
set of int: ncities = 1..3; %  cities, Bristol, Brighton, london
set of int: ncitiesm1 = 1..2; % cities, Bristol, Brighton

% benefits (£k)
array[ndepts,ncities] of int: benefit = array2d(ndepts, ncities, [10,10,0,15,20,
                                         0, 10,15,0,20,
                                         15,0,5,15,0]); 

%  communication costs/unit(£)
array[ncities,ncities] of int: dist = array2d(ncities, ncities, [5,14,13,14,5,9,13,9,10]);

% quantities of communication (k units)
array[ndepts,ndepts] of float: comm = array2d(ndepts, ndepts, 
        [0.0,0.0,1.0,1.5,0.0,
         0.0,0.0,1.4,1.2,0.0,
         0.0,0.0,0.0,0.0,2.0,
         0.0,0.0,0.0,0.0,0.7,
         0.0,0.0,0.0,0.0,0.0]);


% variables
% = 1 iff dept i in cityj 
array[ndepts,ncities] of var 0..1: d; 
% = 1 iff dept i in city j and dept k in city l
array[ndepts,ncities,ndepts,ncities] of var 0..1: g; 

var float: tcost  = sum(i in ndepts, j in ncitiesm1) (int2float(benefit[i,j])*int2float(d[i,j])) -
                  sum(i in ndepts, j in ncities, k in ndepts, l in ncities where k> i) (
                  comm[i,k]*int2float(dist[j,l])*int2float(g[i,j,k,l])
              );

% solve maximize tcost;
solve :: int_search([d[i,j] | i in ndepts, j in ncities], first_fail, indomain_min, complete) maximize tcost;

constraint
   tcost >= 0.0
   /\

   % each dept i located somewhere  	  	
   forall(i in ndepts) (sum(j in ncities) (d[i,j]) = 1)

   /\
   % at most 3 depts in each city
   forall(j in ncities) (sum(i in ndepts) (d[i,j]) <= 3)
   
   /\ % logical relations
   forall(i in ndepts, j in ncities, k in ndepts, l in ncities where k>i) (
     g[i,j,k,l] - d[i,j] <= 0
     /\
     g[i,j,k,l] - d[k,l] <= 0
     /\
     d[i,j] + d[k,l] - g[i,j,k,l] <= 1
     /\
     g[i,j,k,l] <= 1
   )

   /\
   forall(i in ndepts, j in ncities) (d[i,j] <= 1)
;


output [
  "tcost: ", show(tcost), "\n",
  "d:",
] ++ 
[
  if j = 1 then "\n" else " " endif ++
   show(d[i,j])
  | i in ndepts, j in ncities
]  ++ ["\n"]  % ++ ["\ng:"] ++ 
% [
%   if l = 1 /\ k = 1 then "\n" else "" endif ++
%   if l = 1 then "\n\n" else " " endif ++
%    show(g[i,j,k,l])
%   | i in ndepts, j in ncities, k in ndepts, l in ncities
% ]
++ ["\n"]
;
