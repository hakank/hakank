%
% Global constraint assign_and_nvalues in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cassign_and_nvalues.html
% """
%Constraint
%
%    assign_and_nvalues(ITEMS,RELOP,LIMIT)
% 
% Purpose 
%    Given several items (each of them having a specific value that may not be 
%    initially fixed), and different bins, assign each item to a bin, so that 
%    the number n of distinct values in each bin satisfies the condition 
%    n RELOP LIMIT.
% 
% Example
%  (
%  <
%     bin-2	value-3,
%     bin-1	value-5,
%     bin-2	value-3,
%     bin-2	value-3,
%     bin-2	value-4
%     >,<= ,2
% )
% 
% Figure 4.24.1 depicts the solution corresponding to the example. 
% The assign_and_nvalues constraint holds since for each used bin 
% (i.e., namely bins 1 and 2) the number of distinct colours of the 
% corresponding assigned items is less than or equal to the limit 2.
% """

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 


int: n = 5;
array[1..n, 1..2] of 1..6: items = array2d(1..n, 1..2, [
                                           2,3,
                                           1,5,
                                           2,3,
                                           2,3,
                                           2,4
                                       ]);
                                       
var 0..10: limitx;
int: relop = -1; % <=

%
% Since MiniZinc don't handle function variables we use the following 
% hack where t is the type of comparison operator.
% t:
%  - 2 :  a <  b
%  - 1 :  a <= b
%    0 :  a  = b
%    1 :  a >= b
%    2 :  a >  b
% else : a != b
%
predicate cmp(var int: a, var int: b, -2..2: t) = 
   if     t = -2 then
     a < b
   elseif t = -1 then
     a <= b 
   elseif t =  0 then
     a = b
   elseif t =  1 then
     a >= b
   elseif t =  2 then
     a > b
   else 
     a != b
   endif
;

%
% note: items must be fixed.
%
predicate assign_and_nvalues(array[int,int] of int: items, int: relop, var int: limit) =
    let {
       int: low = min([items[i,1] | i in index_set_1of2(items)]),
       int: high = max([items[i,1] | i in index_set_1of2(items)]),
       array[low..high] of var 0..card(index_set_1of2(items)): bins
    } in
   forall(b in low..high) (
      nvalue(bins[b], [items[i,2] | i in index_set_1of2(items) where items[i,1] = b])
      /\
      cmp(bins[b],limit, relop)
   )
 

;

solve satisfy;


constraint
   limitx = 2
   /\
   assign_and_nvalues(items, relop, limitx)   
;


output
[
  "limitx: " ++ show(limitx) ++ "\n" 

];
