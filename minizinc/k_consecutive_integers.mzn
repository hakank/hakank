% 
% K consecutive integers in MiniZinc.
% 
% From StackOverflow
% "k consecutive integers constraint"
% http://stackoverflow.com/questions/18550915/k-consecutive-integers-constraint
% """
% How can I state the following constraint in Constraint Programming? 
% (Preferably in Gurobi or Comet).
%
% S is an array of integers of size n. The set of integers that I can use 
% to fill the array are in the range 1-k. There is a constraint ci for 
% each of the integers that can be used. ci denotes the minimum number of 
% consecutive integers i.
%
% For example if c1 = 3, c2 = 2 then 1112211112111 is not a valid sequence 
% since there must be two consecutive 2's, whereas 1112211122111 is a valid 
% sequence.
% """

% Hmm, this should be alse be possible to use regular.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n;
int: k;

% number of consecutive integers
array[1..k] of int: c;

% decision variables
array[1..n] of var 1..k: x;

% starts[i] = 1 ->  x[i] starts a new sequence
% starts[i] = 0 ->  x[i] is in a sequence
array[1..n] of var 0..k: starts;
% sum of sequences
var 1..n: z = sum([bool2int(starts[i] > 0) | i in 1..n]);


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
% solve :: int_search(x, first_fail, indomain_median, complete) minimize z;

constraint
  assert(k == length(c), "the length of c is not k")
;

constraint

  forall(a in 1..n, b in 1..k) (
    (starts[a] = b ) -> 
                (
                 forall(d in 0..c[b]-1) (x[a+d] = b )
                 /\
                 forall(d in 1..c[b]-1) (starts[a+d] = 0 )
                 /\
                 (if a > 1 then x[a-1] != b else true endif)    % before 
                 /\
                 (if a <= n-c[b] then x[a+c[b]] != b else true endif) % after
                 )
  )
  /\
  % more on starts
  starts[1] > 0 /\
  forall(i in 2..n) (
    starts[i] > 0 <-> ( x[i]!=x[i-1] )
  )
  /\
  forall(i in 1..n) (
     starts[i] > 0 -> x[i] = starts[i]
  )
;

output [ 
  "z     : " ++ show(z) ++ "\n" ++
  "starts: " ++ show(starts) ++ "\n" ++
  "x     : " ++ show(x)  ++ "\n"
];


%
% data
%

%% From the StackOverflow above:
%% unique solution
n = 13;
k = 2;
c = [3,2];

% Other variants
%% unique solution
% n = 12;
% k = 2;
% c = [3,2];

%% no solution 
% n = 14;
% k = 2;
% c = [3,2];

%% 2 solutions
% n = 20;
% k = 2;
% c = [3,2];

%% k=3 
%% 78 solutions
% n = 13;
% k = 3;
% c = [3,2,1];
