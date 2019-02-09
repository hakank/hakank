% 
% Number lock problem in MiniZinc.
% 
% From Presh Talwalkar (MindYourDecisions) 
% """
% Puzzles like this have been shared with the dubious claim that "only a
% genius can solve" them. But they are still fun problems so let's work one
% out.
%
% A number lock requires a 3 digit code. Based on these hints, can you crack
% the code?
%
%  682 - one number is correct and in the correct position
%  645 - one number is correct but in the wrong position
%  206 - two numbers are correct but in the wrong positions
%  738 - nothing is correct
%  780 - one number is correct but in the wrong position
%
% Video:  https://youtu.be/-etLb-8sHBc
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 3;
int: m = 5; % number of hints

array[1..m,1..n] of int: y = array2d(1..m,1..n,
  [
6,8,2, % - one number is correct and in the correct position
6,4,5, % - one number is correct but in the wrong position
2,0,6, % - two numbers are correct but in the wrong positions
7,3,8, % - nothing is correct
7,8,0, % - one number is correct but in the wrong position
  ]);

% How many 
%   pos: correct values and positions
%   val: correct values (regardless if there are correct position or not)
predicate check(array[int] of var int: a, array[int] of var int: b, var int: pos, var int: val) =

  % number of entries in correct position (and correct values)
  sum([a[j] = b[j] | j in 1..length(a)]) = pos /\

  % number of entries which has correct values (regardless if there are in correct position or not)
  sum([a[j] = b[k] | j,k in 1..n ]) = val 
;

% decision variables
array[1..n] of var 0..9: x;

% var 0..n: tpos; % Testing a new number
% var 0..n: tval; % Testing a new number


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  %  682 - one number is correct and in the correct position
  check(y[1,..],x,1,1) /\

  %  645 - one number is correct but in the wrong position
  check(y[2,..],x,0,1) /\

  %  206 - two numbers are correct but in the wrong positions
  check(y[3,..],x,0,2) /\

  %  738 - nothing is correct
  check(y[4,..],x,0,0) /\

  %  780 - one number is correct but in the wrong position
  check(y[5,..],x,0,1)


  % Testing another number to get the correct number of posistions/values
  % /\ check([5,0,2],x,tpos,tval)

;

output [
  "\(x)"
  % , "\ntpos:\(tpos) tval:\(tval)\n"
];

