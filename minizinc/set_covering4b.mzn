%
% Set partition, set covering in MiniZinc.
% 
% same problem as set_covering4.mzn but here we uses sets instead of
% a matrix for representing the objects in the alternatives. This
% representation may be easier to state.
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_alternatives;
int: num_objects;

% costs for the alternatives
array[1..num_alternatives] of int: costs; 

% the alternatives, and their objects
array[1..num_alternatives] of var set of 1..num_objects: a; 

% decision variable: which alternative to choose
array[1..num_alternatives] of var 0..1: x; 

% the objective to minimize
var int: z = sum(i in 1..num_alternatives) (x[i]*costs[i]); 

% solve satisfy;
solve minimize z;

constraint
   % z <= 49 /\ % for solve satisfy, set partition
   % z <= 45 /\ % for solve satisfy, set covering 
   forall(j in 1..num_objects) (
     % all objects must be covered _exactly_ once, set partition
     sum(i in 1..num_alternatives) (x[i] * bool2int(j in a[i])) = 1

     % variant: all objects must be covered _at least_ once, set covering
     % sum(i in 1..num_alternatives) (x[i] * bool2int(j in a[i])) >= 1
   )
;

%
% data
%
num_alternatives =  10;
costs = [ 19, 16, 18, 13, 15, 19, 15, 17, 16, 15];
num_objects = 8;

% the alternatives and the objects they contain
a = [
  {1,6},
  {2,6,8},
  {1,4,7},
  {2,3,5},
  {2,5},
  {2,3},
  {2,3,4},
  {4,5,8},
  {3,6,8},
  {1,6,7}
];


output
[
  "x: " ++ show(x) ++ "\n" ++ 
  "a: " ++ show(a) ++ "\n"
];