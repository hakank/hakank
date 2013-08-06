%
% Set partition, set covering in MiniZinc.
% 
% Example from Lundgren, Rönnqvist, Värbrand "Optimeringslära", page 408.
% 
% We want to minimize the cost of the alternatives which covers all the 
% objects, i.e. all objects must be choosen. The requirement is than an object 
% may be selected _exactly_ once.
%
% Alternative        Cost        Object
% 1                  19           1,6
% 2                  16           2,6,8
% 3                  18           1,4,7
% 4                  13           2,3,5
% 5                  15           2,5
% 6                  19           2,3
% 7                  15           2,3,4
% 8                  17           4,5,8
% 9                  16           3,6,8
% 10                 15           1,6,7
%
% The problem has a unique solution of z = 49 where alternatives 
% 3, 5, and 9 is selected. 
%
% If we, however, allow that an object is selected more than one time, 
% then the solution is z = 45 (i.e. less cost than the first problem),
% and the alternatives 4, 8, and 10 is selected, where object 5 is 
% selected twice (alt. 4 and 8). It's an unique solution as well.
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
array[1..num_alternatives, 1..num_objects] of 0..1: a; 

% decision variable: which alternative to choose
array[1..num_alternatives] of var 0..1: x; 

% the objective to minimize
var int: z = sum(i in 1..num_alternatives) (x[i]*costs[i]); 

% solve satisfy;
solve minimize z;

constraint
%   z <= 49 /\ % for solve satisfy
   % z <= 45 /\
   forall(j in 1..num_objects) (
     % all objects must be covered _exactly_ once, set partition
     % sum(i in 1..num_alternatives) (x[i]*a[i,j]) = 1

     % variant: all objects must be covered _at least_ once, set covering
     sum(i in 1..num_alternatives) (x[i]*a[i,j]) >= 1
   )
;

%
% data
%
num_alternatives =  10;
costs = [ 19, 16, 18, 13, 15, 19, 15, 17, 16, 15];
num_objects = 8;

% the alternatives and the objects they contain
a = 
array2d(1..num_alternatives, 1..num_objects,
[
 % 1 2 3 4 5 6 7 8  the objects 
   1,0,0,0,0,1,0,0,  % alternative 1
   0,1,0,0,0,1,0,1,  % alternative 2
   1,0,0,1,0,0,1,0,  % alternative 3
   0,1,1,0,1,0,0,0,  % alternative 4
   0,1,0,0,1,0,0,0,  % alternative 5
   0,1,1,0,0,0,0,0,  % alternative 6
   0,1,1,1,0,0,0,0,  % alternative 7
   0,0,0,1,1,0,0,1,  % alternative 8
   0,0,1,0,0,1,0,1,  % alternative 9
   1,0,0,0,0,1,1,0,  % alternative 10
]);


output [
  "z: " ++ show(z) ++ "\n" ++ 
  "x: " ++ show(x) ++ "\n" 

];
