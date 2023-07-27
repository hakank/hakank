% 
% Implementation (and small test) of the global constraints 
%    - nvalue 
%    - atleast_nvalue
%    - atmost_nvalue
% in Minizinc.
%
% Reference: 
% Clobal Constraint Catalog
% https://sofdem.github.io/gccat/gccat/Cnvalue.html
% """
% Purpose 
%    NVAL is the number of distinct values taken by the variables of the collection VARIABLES
%
% Example
%     ​(4,​〈3,​1,​7,​1,​6〉)​
%
%     The nvalue constraint holds since its first argument NVAL=4 is set to the number of distinct values occurring within the collection 〈3,​1,​7,​1,​6〉.
% """
%
% References for atleast_nvalue and atmost_nvalue, see below.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 7;
array[1..n] of var 1..n: x; 
var int: nvv; % the count of different values in x

%
% nvalues: count the different values in array x
% If nv = length(x) then nvalues <=> all_different
% 
predicate mynvalue(var int: nv, array[int] of var int: x) = 
  let {
     int: len = length(x),
     var set of lb_array(x)..ub_array(x): s
  }
  in
  % all values in x should be in s
  forall(i in 1..len) (
     x[i] in s
  )
  /\ 
  % secure that the values _not_ in x is _not_ in s
  forall(i in lb_array(x)..ub_array(x)) (
    (not exists(j in 1..len) (x[j] = i ) <-> not (i in s))
  )
  /\
  nv = card(s)
;


% 
% atleast_nvalue 
%
% From Global Constraint Catalog
% https://sofdem.github.io/gccat/gccat/Catleast_nvalue.html
% """
% The number of distinct values taken by the variables of the collection VARIABLES 
% is greater than or equal to NVAL.
% """
%
% Note: nv must be delimited in some way in the calling environment, 
% otherwise it may go astray..
%
predicate atleast_nvalue(var int: nv, array[int] of var int: x) = 
  let {
     var 1..length(x): nv_tmp
  }
  in
  mynvalue(nv_tmp, x) /\
  nv <= nv_tmp
;

%
% atmost_nvalue
%
% From Global Constraint Catalog
% https://sofdem.github.io/gccat/gccat/Catmost_nvalue.html
% """
% The number of distinct values taken by the variables of the collection VARIABLES 
% is less than or equal to NVAL.
% """
predicate atmost_nvalue(var int: nv, array[int] of var int: x) = 
  let {
     var 1..length(x): nv_tmp
  }
  in
  mynvalue(nv_tmp, x) /\
  nv >= nv_tmp
;


% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;
solve satisfy;

constraint
  % The example for nvalue from global constraint site. 
  % The range of x must be changed, though.
  % x =  [3,1,7,1,6] /\ 

  % atleast_nvalue(nvv, x)
  % atmost_nvalue(nvv, x)
  mynvalue(nvv, x)
  /\ nvv = 2
% /\ increasing(x) % symmetry breaking
;

output [
  "nv: ", show(nvv), " x: ", show(x)
];

