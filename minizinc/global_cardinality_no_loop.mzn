% 
% Global constraint global_cardinality_no_loop in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cglobal_cardinality_no_loop.html
% """
% Constraint
%
%     global_cardinality_no_loop​(NLOOP,​VARIABLES,​VALUES)​
% 
% ...
% 
% Purpose
%
%     VALUES[i].noccurrence (1=<i=<|VALUES|) is equal to the number of 
%     variables VARIABLES[j].var(j!=)i, 1=<j=<|VARIABLES|) that are 
%     assigned value VALUES[i].val.
%
%     The number of assignments of the form 
%     VARIABLES[i].var=i(i∈[1,|VARIABLES|]) is equal to NLOOP.

% Example
%     (
%     1, <1, 1, 8,6>,
%     〈val-1 noccurrence-1, val-5 noccurrence-0, val-6 noccurrence-1>
%     )
%
%     The global_cardinality_no_loop constraint holds since:
%     *  Values 1, 5 and 6 are respectively assigned to the set 
%        of variables {VARIABLES[2].var}​ (i.e., 1 occurrence 
%        of value 1), {}​(i.e., no occurrence of value 5) and ​
%        {VARIABLES[4].var}​(i.e., 1 occurrence of value 6). Observe 
%        that, due to the definition of the constraint, the fact that 
%        VARIABLES[1].var is assigned to 1 is not counted.
%     *  In addition the number of assignments of the form 
%        VARIABLES[i].var=i(i∈[1, 4]) is equal to NLOOP=1.
%
% Usage
%
%     Within the context of the tree constraint the 
%     global_cardinality_no_loop constraint allows to model a minimum 
%     and maximum degree constraint on each vertex of our trees.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4; % number of variables
int: m = 3; % number of values
array[1..n] of var 1..8: variables;
array[1..m,1..2] of var 0..8: values;
var 0..n: nloop;


predicate global_cardinality_no_loop(var int: nloop, 
                                     array[int] of var int: variables,
                                     array[int,int] of var int: values) =

   forall(i in index_set_2of2(values)) (
      values[i,2] = sum(j in index_set(variables) where i!=j) (
          bool2int(variables[j]=values[i,1])
      )
   )
   /\
   nloop = sum(j in index_set(variables)) (bool2int(variables[j] = j))

;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
   cp2d(values, array2d(1..m, 1..2, [
                      1,1,
                      5,0,
                      6,1
                   ]))
   % /\
   % cp1d(variables, [1,1,8,6])
   /\
   global_cardinality_no_loop(nloop, variables, values)
  
;

output
[
  "values: " ++ show(values) ++ "\n" ++
  "variables: " ++ show(variables) ++ "\n" ++
  "nloop: " ++ show(nloop) ++ "\n" 
];
