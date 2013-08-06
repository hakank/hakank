%
% Photo problem in Minizinc.
%
% Model used by photo_hkj2_data1.mzn and photo_hkj2_data2.mzn
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";


int: n;        % number of persons
int: num_pref; % number of preferences

array[1..num_pref, 1..2] of 0..n-1: preferences;
array[0..n-1] of var 0..n-1: positions;

var int: z; 

% no solve entry here so we can play as we want...
% solve :: int_search(positions, "first_fail", "indomain", "complete") maximize z;
% solve :: int_search(positions, "first_fail", "indomain", "complete") satisfy;

% helper predicate
predicate nextTo(var 1..n: a, var 1..n: b) = 
   abs(a - b) = 1 
   % \/ % for a circular array, fisheye photo? :-)
   % ( (a = 1 /\ b = n) \/ (a = n /\ b = 1)  )
; 


constraint 
  all_different(positions) 
  /\
  z = sum(i in 1..num_pref) ( 
        bool2int(nextTo(positions[preferences[i,1]], positions[preferences[i,2]]))
      )
;

