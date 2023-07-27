% 
% Global constraint distance_between in MiniZinc.
% 
% Global Constraints Catalogue
% 
% https://sofdem.github.io/gccat/gccat/Cdistance_between.html
% """
% Let Ui and Vi be respectively the ith and jth variables (i!=j) of 
% the collection VARIABLES1. In a similar way, let Xi and Yi be 
% respectively the ith and jth variables ​(i!=j)​ of the collection 
% VARIABLES2. DIST is equal to the number of times one of the 
% following mutually incompatible conditions are true:
%  *  Ui CTR Vi holds and Xi CTR Yi does not hold,
%  *  Xi CTR Yi holds and Ui CTR Vi does not hold.
% Example
%     (2, <3, 4, 6, 2, 4>, <2, 6, 9, 3, 6>, <)
%
% The distance_between constraint holds since the following DIST=2 conditions are verified:
% *  VARIABLES1[4].var=2 <  VARIABLES1[1].var=3 /\
%    VARIABLES2[4].var=3 >= VARIABLES2[1].var=2
% *  VARIABLES2[1].var=2 <  VARIABLES2[4].var=3 /\
%    VARIABLES1[1].var=3 >= VARIABLES1[4].var=2
% """

% 
% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 5;
array[1..n] of var 1..9: x;
array[1..n] of var 1..9: y;
var int: d; % the distance

% solve satisfy;
solve maximize d;


%
% Since MiniZinc don't handle function variables
% have the following hack where t is the type of comparison operator
% t:
%  - 2 :  a <  b
%  - 1 :  a <= b
%    0 :  a  = b
%    1 :  a >= b
%    2 :  a >  b
%
predicate cmp(var int: a, var int: b, int: t) = 
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
     true
   endif
;

%
% distance_between
% 
% Note the cmp hack with a type for the operator.
%
predicate distance_between(var int: d, array[int] of var int: x, array[int] of var int: y, int: t) =

  assert(index_set(x) = index_set(y), "index_set of x and y must be equal",

     d = sum(i,j in index_set(x) where i != j) (
       
        bool2int(
         (
           cmp(x[i],x[j], t)
           /\
           not(cmp(y[i],y[j], t))
         )
         \/
         (
           not(cmp(x[i], x[j], t))
           /\
           cmp(y[i], y[j],t )
         )
        )
     )

  
  ) % end assert
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint
  cp1d(x, [3,4,6,2,4])
  /\
  cp1d(y, [2,6,9,3,6])
  /\
  distance_between(d, x, y, -2)

  %  /\
  %  d = 2
 
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "d: " ++ show(d) ++ "\n"
];
