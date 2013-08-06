% 
% Bin packing problem in MiniZinc.
% 
% This model is a port from GLPK:s example bpp.mod. 
% See the Note below.
%
% """
% Given a set of items I = {1,...,m} with weight w[i] > 0, the Bin
% Packing Problem (BPP) is to pack the items into bins of capacity c
% in such a way that the number of bins used is minimal. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Note: The GLPK model bpp.mod use the matrix z as a heuristic for calculating 
%       the upper value of number of bins to use (the parameter n).
%
%       This don't work in MiniZinc since all array dimensions must be fixed.
%       Or rather: I could not translate the z heuristic to a initialization 
%       without getting errors of cyclic definition of z.
%
%       Instead, this model declares z as var int and uses the same logic in
%       the constraints section. This is, however, commented for now.
%       
%       Without the "z heuristics" the model become a standard bin packing
%       problem.
%

% number of items 
int: m;

% set of items 
set of int: I = 1..m;

% w[i] is weight of item i 
array[I] of int: w;

% bin capacity 
int: c;


% """
% We need to estimate an upper bound of the number of bins sufficient
% to contain all items. The number of items m can be used, however, it
% is not a good idea. To obtain a more suitable estimation an easy
% heuristic is used: we put items into a bin until it is possible, and
% if the bin is full, we use another bin. Thus, the number of bin used
% in this way gives us a more appropriate estimation. 
% """
% z[i,j] = 1 if item i is in bin j, otherwise z[i,j] = 0 
% array[I, J] of var 0..1: z;

% """
% determine the number of bins used by the heuristic; obviously it is
% an upper bound of the optimal solution 
% """
% var int: n = sum(j in 1..m) ( exists(i in I) (z[i,j] = 1));
int: n = m;


% set of bins 
set of int: J = 1..n;

% x[i,j] = 1 means item i is in bin j 
array[I, J] of var 0..1: x;

% used[j] = 1 means bin j contains at least one item 
array[J] of var 0..1: used;

array[J] of var 0..c: bin_total;

% objective is to minimize the number of bins used 
var int: obj =  sum(j in J) (used[j]);
solve :: int_search(
        [x[i,j] | i in I, j in J] ++
%        [z[i,j] | i in I, j in 1..m] ++
        used ++
        bin_total
        , 
        first_fail, indomain_min, complete) minimize obj;

constraint

% % note: skipping all the z heuristics
%   forall(i in I, j in 1..m) (
%     (
%       % put item 1 into bin 1 
%       (i = 1 /\ j = 1 -> z[i,j] = 1)
%
%       /\ 
%       % if item i is already in some bin, do not put it into bin j 
%       ( (sum(jj in 1..j-1) (bool2int(z[i,jj] = 1)) > 0 -> z[i,j] = 0))
%
%       /\
%       % if item i does not fit into bin j, do not put it into bin j 
%       ( sum(ii in 1..i-1) (w[ii] * z[ii,j] + w[i]) > c -> z[i,j] = 0)
%     ) 
%     \/
%     z[i,j] = 1
%   )
%
%   /\
%    % each item must be exactly in one bin 
%    forall(i in I) (sum(j in J) (z[i,j]) = 1)
%
%    /\
%    % no bin must be overflowed 
%    forall(j in 1..m) (sum(i in I) (w[i] * z[i,j]) <= c)
%
%
%  /\
   % each item must be exactly in one bin 
   forall(i in I) ( sum(j in J) (x[i,j]) = 1)

   /\
   % if bin j is used, it must not be overflowed 
   forall(j in J) (sum(i in I) (w[i] * x[i,j]) <= c * used[j])

   /\
   forall(j in J) (
       bin_total[j] >= 0
       /\
       bin_total[j] =  sum(i in I) (w[i] * x[i,j])
  )
  
;

%
% data
%
m = 6;
w = [50, 60, 30, 70, 50, 40];
c = 100;

% "The optimal solution is 3 bins "
output [ "\nx:"] ++
[
  if j = 1 then "\n" else " " endif ++ 
    show(x[i,j])
  | i in I, j in J
] ++
% ["\nz: "] ++
% [
%   if j = 1 then "\n" else " " endif ++ 
%     show(z[i,j])
%   | i in I, j in 1..m
% ] ++
[
 "\nused: ", show(used), "\n",
 "bin_total: ", show(bin_total), "\n",
 "obj: ", show(obj), "\n",
]
