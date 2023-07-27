% 
% Global constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csame_interval.html
% """
% same_interval​(VARIABLES1,​VARIABLES2,​SIZE_INTERVAL)​
%
% Purpose
% 
% Let Ni (respectively Mi) denote the number of variables of the collection VARIABLES1 
% (respectively VARIABLES2) that take a value in the interval 
% [SIZE_INTERVAL*i, SIZE_INTERVAL*i+SIZE_INTERVAL-1. For all integer i we have Ni=Mi.
%
% Example
%     (
%     <1, 7, 6, 0, 1, 7>,
%     <8, 8, 8, 0, 1, 2>, 3
%     )
%
% In the example, the third argument SIZE_INTERVAL=3 defines the following family of 
% intervals [3*k, 3*k+2], where k is an integer. Consequently the values of the collection 
% <1, 7, 6, 0, 1, 7> are respectively located within intervals  
% [0, 2], [6, 8], [6, 8], [0, 2], [0, 2], [6, 8]. 
% Therefore intervals [0, 2] and [6, 8] are respectively used 3 and 3 times. Similarly, 
% the values of the collection <8, 8, 8, 0, 1, 2> are respectively located within intervals  
% [6, 8], [6, 8], [6, 8], [0, 2], [0, 2], [0, 2]. As before intervals  
% [0, 2] and [6, 8] are respectively used 3 and 3 times. Consequently the same_interval 
% constraint holds. Figure 4.244.1 illustrates this correspondence.
% 
%  [See the figure at https://sofdem.github.io/gccat/gccat/Csame_interval.html]
% 
% Figure 4.244.1. Correspondence between the intervals associated with collection 
% <1, 7, 6, 0, 1, 7> and with collection <8, 8, 8, 0, 1, 2>
%
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 6;
array[1..n] of var 0..8: variables1;
array[1..n] of var 0..8: variables2;

var 1..n: size_interval; % = 3; 

predicate same_interval(var int: size_interval, 
                        array[int] of var int: variables1,
                        array[int] of var int: variables2) =

   forall(i in 0..ub(size_interval)) (

      sum(k in index_set(variables1)) ( 
                           bool2int(variables1[k] >= i*size_interval /\
                                    variables1[k] <= i*size_interval+size_interval-1
                                   )
                        )
      =  

      sum(k in index_set(variables2)) ( 
                           bool2int(variables2[k] >= i*size_interval /\
                                    variables2[k] <= i*size_interval+size_interval-1
                                   )
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
    cp1d(variables1,[1,7,6,0,1,7])
    /\
    cp1d(variables2, [8,8,8,0,1,2])
    /\
    same_interval(size_interval, variables1, variables2)
    /\
    size_interval = 3

;

output [
  "variables1: ", show(variables1), "\n",
  "variables2: ", show(variables2), "\n",
  "size_interval: ", show(size_interval), "\n",
] 
++ ["\n"]

;
