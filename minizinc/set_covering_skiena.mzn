%
% Set covering in MiniZinc.
%
% Example from Steven Skiena, The Stony Brook Algorithm Repository
% http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
% """
% Input Description: A set of subsets S_1, ..., S_m of the 
% universal set U = {1,...,n}.
% 
% Problem: What is the smallest subset of subsets T subset S such that \cup_{t_i in T} t_i = U?
% """
% Data is from the pictures INPUT/OUTPUT.

% Solution: Sets 3,6,7. Total elements choosen: 15
% Another solution with three sets is {4,6,7}, but the total choosen elements is 17.

int: num_sets;   % number of sets
int: num_elements; % number of elements
array[1..num_sets, 1..num_elements] of 0..1: belongsx;

array[1..num_sets] of var 0..1: x;
var int: z = sum(j in 1..num_sets) (x[j]); % number of choosen sets 
var int: tot_elements; % total number of elements in the choosen sets

% solve satisfy;
solve minimize z;
% solve minimize tot_elements;
% solve minimize z + tot_elements;

predicate set_covering(array[int, int] of int: matrix, array[int] of var 0..1: x) =
 forall(j in index_set_2of2(matrix)) (
     sum(i in index_set_1of2(matrix)) (belongsx[i,j]*x[i])>=1
 )
 /\
 tot_elements = sum(i in index_set_1of2(matrix)) (
    sum(j in 1..num_elements) (x[i]*belongsx[i,j])
 )

;

constraint
  set_covering(belongsx, x)
  % /\
  % z <= 3
  % /\ tot_elements <= 12

;

%
% data
% 
num_sets = 7;
num_elements = 12;
belongsx = 
  array2d(1..num_sets, 1..num_elements, [
   %  1 2 3 4 5 6 7 8 9 0 1 2  elements
      1,1,0,0,0,0,0,0,0,0,0,0,      % Set 1
      0,1,0,0,0,0,0,1,0,0,0,0,      %     2
      0,0,0,0,1,1,0,0,0,0,0,0,      %     3
      0,0,0,0,0,1,1,0,0,1,1,0,      %     4
      0,0,0,0,0,0,0,0,1,1,0,0,      %     5
      1,1,1,0,1,0,0,0,1,1,1,0,      %     6
      0,0,1,1,0,0,1,1,0,0,1,1,      %     7
]);

output
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" ++
  "tot_elements: " ++ show(tot_elements) ++ "\n"
];
