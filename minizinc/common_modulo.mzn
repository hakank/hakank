% 
% Global constraint common_modulo in MiniZinc. 
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ccommon_modulo.html
% """
% Constraint
%     common_modulo (NCOMMON1, NCOMMON2, VARIABLES1, VARIABLES2, M) 
%
% Purpose
%
% NCOMMON1 is the number of variables of the collection of variables 
% VARIABLES1 taking a value situated in an equivalence class 
% (congruence modulo a fixed number M) derived from the values assigned 
% to the variables of VARIABLES2 and from M.
%
% NCOMMON2 is the number of variables of the collection of variables 
% VARIABLES2 taking a value situated in an equivalence class 
% (congruence modulo a fixed number M) derived from the values assigned 
% to the variables of VARIABLES1 and from M.
%
% Example
%     (
%     3, 4, <0, 4, 0, 8>, 
%     <7, 5, 4, 9, 2, 4>, 5
%     )
%
% In the example, the last argument M=5 defines the equivalence classes 
% a=0 (mod 5), a=1 (mod 5), a=2 (mod 5), a=3 (mod 5), and 
% a=4 (mod 5) where a is an integer. As a consequence the items of 
% collection <0, 4, 0, 8> respectively correspond to the equivalence 
% classes a=0 (mod 5), a=4 (mod 5), a=0 (mod 5), and a=3 (mod 5). 
% Similarly the items of collection <7, 5, 4, 9, 2, 4> respectively 
% correspond to the equivalence classes a=2 (mod 5), a=0 (mod 5), 
% a=4 (mod 5), a=4 (mod 5), a=2 (mod 5), and a=4 (mod 5). 
% The common_modulo constraint holds since:
%
% * Its first argument NCOMMON1=3 is the number of equivalence 
%   classes associated with the items of collection <0, 4, 0, 8> 
%   that also correspond to equivalence classes associated with 
%   <7, 5, 4, 9, 2, 4>.
% * Its second argument NCOMMON2=4 is the number of equivalence classes 
%   associated with the items of collection <7, 5, 4, 9, 2, 4> that 
%   also correspond to equivalence classes associated with <0, 4, 0, 8>.
% """

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

array[1..4] of var 0..9: x;
array[1..6] of var 0..9: y;

int: m = 5; % modulo
var int: a;
var int: b;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

%
% helper predicate to convert an array to a set
%
predicate array2set_mod(array[int] of var int: x, var set of int: s, int: m) =

   forall(i in index_set(x)) ( (x[i] mod m) in s ) /\
   % secure that the values _not_ in x is _not_ in s
   forall(i in 0..m-1) (
     (not exists(j in index_set(x)) (x[j] mod m = i ) <-> not (i in s))
   )
;


%
% common(a, b, x, y)
%  - a is the number of values of x that are in y
%  - b is the number of values of y that are in x
%
predicate common_modulo(var int: a, var int: b, array[int] of var int: x, array[int] of var int: y, int: m) = 
   let {
     var set of 0..m-1: x_set,
     var set of 0..m-1: y_set
   }
   in
   array2set_mod(x, x_set,m) /\
   array2set_mod(y, y_set,m) /\
   a = sum(i in index_set(x)) (
           bool2int((x[i] mod m) in y_set)
       )
   /\
   b = sum(j in index_set(y) ) (
           bool2int((y[j] mod m) in x_set)
       )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


constraint
   cp1d([0,4,0,8],x) /\
   cp1d([7,5,4,9,2,4],y) /\

   common_modulo(a, b, x, y, m)

   % The "reverse problem" works as well, i.e. letting x and y be unknown
   % and fix a and b.
   % /\ a = 3
   % /\ b = 4

   % /\ increasing(x)
   % /\ increasing(y) 
;



output
[
  "a: " ++ show(a) ++ "\n" ++
  "b: " ++ show(b) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "m: " ++ show(m) ++ "\n" 
]
