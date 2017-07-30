% 
% Another color problem in Minizinc
% (cf color_simple.mzn)
%
%
% Map coloring example in Zinc
% By Reza Rafeh July 2005
% This model contains no error
% Main features: Enumerated types
% 
% Note: Since Minizinc don't have enums I use an array.
%
% This version uses a symmetry breaking with precedence/1.

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";


int: n; % number of countries
array[1..n] of var 1..4: country; % = [belgium, denmark, france, germany, netherlands, luxembourg];
array[1..n,1..n] of 0..1: connections; % the map (as a matrix)

% var int: ss = sum(i in 1..n) (i*country[i]); % bara en test

% The map (from an earlier version of the model)
%constraint
%	country_colour[france] 		!=	country_colour[belgium]
%  /\	country_colour[france] 		!= 	country_colour[luxembourg]
%  /\	country_colour[france] 		!= 	country_colour[germany]
%  /\	country_colour[luxembourg] 	!= 	country_colour[germany]
%  /\	country_colour[luxembourg] 	!= 	country_colour[belgium]
%  /\	country_colour[netherlands]	!= 	country_colour[belgium]
%  /\	country_colour[germany]		!= 	country_colour[belgium]
%  /\	country_colour[germany]		!= 	country_colour[netherlands]
%  /\	country_colour[germany] 	!= 	country_colour[denmark]
%;

%
%  coloring(connections, num_countries)
%    where
%      conn: an indicence matrix: 1 for connected, 0 for not connected
%                   (no self-loops)
%      contries: the array of the countries (country flag) to be solved
%      num_countries: the number of countries    
%
predicate coloring(array[int,int] of int: conn, array[int] of var int: countries, int: num_countries) =
           forall(i,j in 1..num_countries where i < j /\ conn[i,j] = 1) (
              countries[i] != countries[j]
           )
;

%
% does a contains e?
%
predicate contains(var int: e, array[int] of var int: a) =
   exists(i in index_set(a)) (
      a[i] = e
   )
;

%
% precedence
% 
% Note: This implementation is different from Walsh's suggestion.
% We can't assume that all values in lb_array(x)..ub_array(x) 
% is in x, though if a value (i) exist, then all 1..i-1 must
% be in x.
predicate precedence_me(array[int] of var int: x) =
   let {
      int: lba = lb_array(x),
      int: uba = ub_array(x),
      int: len = length(x)
   }
   in
   % must contain 1 (lb)
   contains(lba, x)
   /\
   forall(i in lba..uba) (
      contains(i, x) ->
      (
       exists(j in lba..len) (
         x[j] = i % get index of i
         /\ 
         forall(k in lba..j-1) (x[k] <= i)
      )
    )
    /\
    forall(k in lba..i-1) (contains(k, x))

  )
;


%
% Without symmetry breaking: 144 solutions.
% With the precedence symmetry breaking: 6 solutions.
constraint 
    coloring(connections, country, n)
    %/\
    %precedence_me(country)
;


solve satisfy;
% solve minimize sum(i in 1..n) (i*country[i]);

%
% data
%
n = 6; % number of countries

%
% The map as a matrix
%
% [belgium, denmark, france, germany, netherlands, luxembourg]
connections = 
array2d(1..n, 1..n, 
[    0,   0,   1,   1,   1,   1,
     0,   0,   0,   1,   0,   0,
     1,   0,   0,   1,   1,   0,
     1,   1,   1,   0,   1,   1,
     1,   0,   1,   1,   0,   0,
     1,   0,   0,   1,   0,   0
]);

output [
   "[belgium, denmark, france, germany, netherlands, luxembourg]\n",
   show(country), "\n" % ," : ", show(ss), "\n"
   
]
;
