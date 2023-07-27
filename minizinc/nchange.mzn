% 
% Global constraint nchange in MiniZinc.
% 
% 
% From http://www.it.uu.se/research/group/astra/CTcourse03/Slides/Routing.pdf
% page 48
%
% """
% NCHANGE is the number of times that CTR holds on
% consecutive variables of the collection VARIABLES.
% 
% nchange(3,[4,4,3,4,1])
% """
% since, 4 != 3, 3!=4,  4 != 1 (three changes)
%
% See also  Global Constraint Catalogue 
% https://sofdem.github.io/gccat/gccat/Cchange.html
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


include "globals.mzn"; 

int: n; % size of arrays
array[1..n] of var int: x; % decision variable

% Testing a "secondary" constraint to locate the changes.
array[1..n-1] of var 0..1: c_pos;

% number of changes
var int: num_changes;


%
% nchange: Default compare operator is !=
%
predicate nchange(var int: c, array[int] of var int: x) =
  nchange_cmp(c, x, 0)
;

%
% nchange with compare operators:
%   0: !=
%   -1: x[i-1] < x[i]
%   1: x[i-1] > x[i]
%
predicate nchange_cmp(var int: c, array[int] of var int: x, int: cmp) =
  if cmp = 0 then 
     c = sum(i in 2..length(x)) ( bool2int(x[i-1] != x[i]) )
  elseif cmp = -1 then
     c = sum(i in 2..length(x)) ( bool2int(x[i-1] < x[i]) )
  elseif cmp = 1 then
     c = sum(i in 2..length(x)) ( bool2int(x[i-1] > x[i]) )
  else
     true
  endif
;

solve satisfy;

constraint
  x = [4,4,3,4,1] % for n = 5
  /\
  forall(i in 1..n) (
    x[i] >= 1 
    /\ x[i] <= n % some arbitrary upper limit (otherwise the sky's the limit)
  )
  /\
  forall(i in 1..n-1) (
     c_pos[i] = 1 <-> x[i] != x[i+1]
  )
  /\
  nchange(num_changes, x)
  % /\
  % num_changes = 2

  %  /\ % symmetry breaking
  %  increasing(x)

  % /\ % testing the positions of the changes
  % c_pos = [0,1,1,0]
  %   /\
  %  nchange(1, c_pos)
  %  /\
  %  increasing(c_pos)


;


%
% data
%
n = 5;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "c_pos: " ++ show(c_pos) ++ "\n" ++
  "num_changes: " ++ show(num_changes) ++ "\n"
];
