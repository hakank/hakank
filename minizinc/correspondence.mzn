% 
% Global constraint correspondence in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ccorrespondence.html
% """
% The variables of the TO collection correspond to the variables of the FROM 
% collection according to the permutation expressed by PERMUTATION.
%
% Example
%   (
%   <1,​9,​1,​5,​2,​1>,​
%   <6,​1,​3,​5,​4,​2>,​
%   <9,​1,​1,​2,​5,​1>
%   )
%
% As illustrated by Figure 4.65.1, the correspondence constraint holds since:
%
% * The first item FROM​[1]​.from=1 of collection FROM corresponds to the 
%   PERMUTATION​[1].var=6th item of collection TO.
% * The second item FROM​[2]​.from=9 of collection FROM corresponds to the
%   PERMUTATION​[2].var=1th item of collection TO.
% * The third item FROM​[3]​.from=1 of collection FROM corresponds to the 
%   PERMUTATION​[3].var=3th item of collection TO.
% * The fourth item FROM​[4]​.from=5 of collection FROM corresponds to the 
%   PERMUTATION​[4].var=5th item of collection TO.
% * The fifth item FROM​[5]​.from=2 of collection FROM corresponds to the 
%   PERMUTATION​[5].var=4th item of collection TO.
% * The sixth item FROM​[6]​.from=1 of collection FROM corresponds to the 
%   PERMUTATION​[6].var=2th item of collection TO.
% """
% 

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 6;
array[1..n] of var 1..9: from;
array[1..n] of var 1..n: permutation;
array[1..n] of var 1..9: to;

%
% correspondence(FROM, PERMUTATION, TO)
%
predicate correspondence(array[int] of var int: from, array[int] of var int: permutation, array[int] of var int: to) = 

   all_different(permutation)
  /\
  forall(i in index_set(from)) (
      from[i] = to[permutation[i]]
  )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


solve satisfy;

constraint

  cp1d([1,9,1,5,2,1], from)
  /\
  cp1d([6,1,3,5,4,2], permutation)
  /\
  cp1d([9,1,1,2,5,1], to)
  /\
  correspondence(from, permutation, to)

;

output
[
  "from: " ++ show(from) ++ "\n" ++
  "permutation: " ++ show(permutation) ++ "\n" ++
  "to: " ++ show(to) ++ "\n" 
];
