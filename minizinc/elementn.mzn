% 
% Global constraint elementn in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Celementn.html
% """
% Constraint
%
%     elementn(INDEX,TABLE,ENTRIES)
%
% Purpose
%
%    forall(i)[1,|ENTRIES|]:ENTRIES[i].entry=TABLE[INDEX+i-1].value
%
% Example
%     (3,<6,9,2,9>,<2,9>)
%
% The elementn constraint holds since its third argument ENTRIES=<2,9> 
% is set to the subsequence starting at the third (i.e., INDEX=3) item 
% of the table TABLE=<6,9,2,9>.
% Usage
%
% The elementn constraint is useful for extracting of subsequence of 
% fixed length from a given sequence.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
int: s = 2; % length of sub sequence (must be fixed)

array[1..n] of var 1..9: elements;
array[1..s] of var 1..9: entries;
var 1..n: t_index;

%
% Note: this use a var t_index, which makes is quite multidirectional.
%
predicate elementn(var int: t_index, 
                   array[int] of var int: elements, 
                   array[int] of var int: entries) =
    forall(i in index_set(entries)) (
       entries[i] = elements[t_index+i-1]
    )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
    cp1d(elements, [6,9,2,9])
    % cp1d(elements, [2,9,2,9]) % finds both occurrences
    /\
    cp1d(entries, [2,9])
    /\
    t_index = 3
    /\
    elementn(t_index, elements, entries)

;

output
[
  "elements: " ++ show(elements) ++ "\n" ++
  "entries: " ++ show(entries) ++ "\n" ++
  "t_index: " ++ show(t_index) ++ "\n"
];
