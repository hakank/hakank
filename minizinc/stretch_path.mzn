% 
% Global constraint stretch_path in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cstretch_path.html
% """
% Let n be the number of variables of the collection VARIABLES.
% Let Xi, ..., Xj  (1<=i<=j<=n)  be consecutive variables of the 
% collection of variables VARIABLES such that the following conditions apply:
%
% * All variables Xi, ..., Xj take a same value from the set of values 
%   of the val attribute,
% * i=1 or Xi-1 is different from Xi,
% * j=n or Xj+1 is different from Xj.
%
% We call such a set of variables a stretch. The span of the stretch is 
% equal to j-i+1, while the value of the stretch is Xi. 
% An item  (val-v, lmin-s, lmax-t)  gives the minimum value s as well 
% as the maximum value t for the span of a stretch of value v.
% 
% Example
%     (
%     <
%     var-6, 
%     var-6, 
%     var-3, 
%     var-1, 
%     var-1, 
%     var-1, 
%     var-6, 
%     var-6
%     >, 
%     <
%     val-1	lmin-2	lmax-4, 
%     val-2	lmin-2	lmax-3, 
%     val-3	lmin-1	lmax-6, 
%     val-6	lmin-2	lmax-2
%     >
%     )
%
% The stretch_path constraint holds since the sequence 6 6 3 1 1 1 6 6 contains 
% four stretches 6 6, 3, 1 1 1, and 6 6 respectively verifying the following 
% conditions:
% 
% * The span of the first stretch 6 6 is located within interval  [2, 2]  
%    (i.e., the limit associated with value 6).
% * The span of the second stretch 3 is located within interval  [1, 6]  
%   (i.e., the limit associated with value 3).
% * The span of the third stretch 1 1 1 is located within interval  [2, 4]  
%   (i.e., the limit associated with value 1).
% * The span of the fourth stretch 6 6 is located within interval  [2, 2]  
%   (i.e., the limit associated with value 6).
% """

% Originally defined by G. Pesant in 
% "A Filtering Algorithm for the Stretch Constraint"

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 8; % length of sequence
int: m = 4; % length of stretch constraints

set of int: S = {1,2,3,6};
array[1..n] of var S: x;
array[1..m] of var S: val;
array[1..m] of var 1..n: lmin;
array[1..m] of var 1..n: lmax;

%
% stretch_path(variables, val, lmin, lmax)
%
predicate stretch_path(array[int] of var int: x, 
                       array[int] of var int: val, 
                       array[int] of var int: lmin, 
                       array[int] of var int: lmax) =
     let {
          int: lbx = min(index_set(x)),
          int: ubx = max(index_set(x)),
          int: lbval = min(index_set(val)),
          int: ubval = max(index_set(val)),          
          array[lbx..ubx] of var 0..1: first_pos % first positions of a stretch
     }
     in
     % sanity clause 
     forall(s in lbval..ubval) (
       lmin[s] <= lmax[s]
     )
     /\
     % get the index for first positions of the stretches
     first_pos[1] = 1 /\
     forall(i in lbx+1..ubx) (
       first_pos[i] = 1 <-> (x[i] != x[i-1])
     )
     /\ 
     % check all values in x
     forall(i in lbx..ubx) (
        let {
           var lbval..ubval: s_ix  % index of the current value in the stretch
        }
        in
        val[s_ix] = x[i]
        /\
        (
         % ignore if we are inside a stretch
         first_pos[i] = 0
         \/
         (
          % when in first position in a stretch, then
          % check that this stretch has is OK
          first_pos[i] = 1 
          <->
          (
            exists(j in i..ubx) (
              % check the interval
              j-i+1 >= lmin[s_ix] /\ 
              j-i+1 <= lmax[s_ix] 

              /\ % all same values in the stretch 
              forall(k in i+1..j) (
                x[k] = x[i]
                %/\ first_pos[k] = 0
            )
            /\ % the neighbours of the stretch are different
            (i > lbx -> x[i-1] != x[i])
            /\
            (j < ubx -> x[j+1] != x[j])
           )
         )
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
  % cp1d(x, [6, 6, 3, 1, 1, 1, 6, 6])
  % /\
  cp1d(val, [1,2,3,6])
  /\
  cp1d(lmin, [2,2,1,2])
  /\
  cp1d(lmax, [4,3,6,2])
  /\ 
  stretch_path(x, val, lmin, lmax)   

;


output [
  "x: " ++ show(x) ++ "\n" ++
  "val: " ++ show(val) ++ "\n" ++
  "lmin: " ++ show(lmin) ++ "\n" ++
  "lmax: " ++ show(lmax) ++ "\n"
];
