% 
% Global constraint global_cardinality_low_up in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Copen_global_cardinality_low_up.html
% """
% Constraint
%
%     open_global_cardinality_low_up(S,​VARIABLES,​VALUES)​
%
% Purpose
%
%     Each value VALUES[i].val(1<=i<=|VALUES|)​should be taken by at 
%     least VALUES[i].omin and at most VALUES[i].omax variables of the 
%     VARIABLES collection for which the corresponding position 
%     belongs to the set S.
%
% Example
%     (
%     ​{2,​3,​4}​,​
%     <3,​3,​8,​6>,​
%     <
%     val-3 omin-1 omax-3,​
%     val-5 omin-0 omax-1,​
%     val-6 omin-1 omax-2
%     >
%     )
%
%     The open_global_cardinality_low_up constraint holds since:
%      * Values 3, 5 and 6 are respectively used 1 (1<=1<=3), 
%        0 (0<=0<=1) and 1 (1<=1<=2) times within the collection 
%        <3,​3,​8,​6> (the first item 3 of <3,​3,​8,​6> is ignored 
%        since value 1 does not belong to the first argument 
%        S=​{2,​3,​4}​ of the open_global_cardinality_low_up constraint).
%      * No constraint was specified for value 8.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
int: m = 3;
array[1..n] of var 1..8: variables;
var set of 1..n: s;
% Since we are using values[i,2] and values[i,3] as range this must
% be a par array.
array[1..m, 1..3] of 0..6: values = array2d(1..m, 1..3, [
                                                             3,1,3,
                                                             5,0,1,
                                                             6,1,2
                                                         ]);
                                                     

predicate open_global_cardinality_low_up(var set of int: s, 
                                         array[int] of var int: variables,
                                         array[int,int] of int: values) =
  forall(i in index_set_1of2(values)) (
          let {
              var int: ssum
          } in
          ssum = sum(j in index_set(variables)) (
                             bool2int( 
                                j in s /\
                                variables[j] = values[i,1]
                             )
                        )
          /\
          ssum in values[i,2]..values[i,3]
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
;

solve satisfy;

constraint
   s = {2,3,4} /\
   cp1d(variables, [3,3,8,6]) /\
   open_global_cardinality_low_up(s, variables, values)

;


output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "values: " ++ show(values) ++ "\n" ++
  "s: " ++ show(s) ++ "\n"
];
