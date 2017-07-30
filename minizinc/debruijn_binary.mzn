% 
% Implementation of de Bruijn sequences in Minizinc, both "classical" and "arbitrary". 
%
% Compare with the the web based programs:
%   http://www.hakank.org/comb/debruijn.cgi   
%   http://www.hakank.org/comb/debruijn_arb.cgi
%
% This Minizinc program was written by Hakan Kjellerstrand, hakank@bonetmail.com,
% and is commented in the (swedish) blog post
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% Note: the pow() function should be able to take integer values but it doesn't in Minizinc (0.7.1), 
% hence the ceil and int2float stuff. 

% for the global constraints 
%   - all_different
%   - global_cardinality
include "globals.mzn"; 


% int: base = 13;  % the base to use, i.e. the alphabet 0..base-1
% int: n    = 4;  % number of bits to use (n = 4 -> 0..base^n-1 = 0..2^4 -1, i.e. 0..15)
% int: m    = 52;  % the length of the sequence. For "arbitrary" de Bruijn sequences

int: base = 2;  % the base to use, i.e. the alphabet 0..base-1
int: n    = 5;  % number of bits to use (n = 4 -> 0..base^n-1 = 0..2^4 -1, i.e. 0..15)
int: m    = 27;  % the length of the sequence. For "arbitrary" de Bruijn sequences


% int: m = ceil(pow(int2float(base), int2float(n)));  % the length of the sequence k^n. Use this for "classic" de Bruijn sequences

array[1..m] of var 0..ceil(pow(int2float(base), int2float(n)))-1: x; 
array[1..m, 1..n] of var 0..base-1: binary;              % binary representation of the numbers
array[1..m] of var 0..base-1: bin_code;                  % the sequence in base-representation
array[0..base-1] of var int: gcc;                        % number of occurrences of the values in bin_code

%
% some solve variants:
%
% solve satisfy;
% solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

% This was used with the eclipse solver for a fast solution (65 solutions in 5 seconds) for
% the following problem.
%   base:  4
%   n   :  3
%   m   : 52
% Change the parameters in credit and bbs for more results
% solve :: int_search(x, first_fail, indomain_split, credit(5,bbs(1))) satisfy;
% solve :: int_search(x, first_fail, indomain_random, complete) satisfy;
solve satisfy;


%
% converts a number (s) <-> an array of numbers (t) in the specific base.
%
predicate toNum(array[int] of var 0..base-1: t, var int: s,  int: base) =
          let { int: len = length(t) }
          in
          s = sum(i in 1..len) (
            pow(base, len-i) * t[i]
          )
          % /\ forall(i in 1..len) (t[i] >= 0)
;


%
% the constraints
%
constraint

    % all number must be different
    all_different(x) 

    /\ % symmetry breaking: the minimum element should be the first element
    minimum(x[1], x)

    /\ % converts x <-> binary (for all_different(x) )
    forall(i in 1..m) (
       % toNum( [binary[i,j] | j in 1..n], x[i], int2float(base)) % this should work, but don't
       let {
          array[1..n] of var 0..base-1: t
       }
       in
       toNum(t, x[i], base) /\
       forall(j in 1..n) (
         binary[i,j] = t[j]
        )
    )
    /\ % the de Bruijn condition: 
       % the first elements in binary[i] is the same as the last elements in binary[i-1]
      forall(i in 2..m) (
        forall(j in 2..n) ( binary[i-1, j] = binary[i, j-1] )
     ) 
    /\ % ... and around the corner
    forall(j in 2..n) ( binary[m, j] = binary[1, j-1] )

    %
    % The constraints below is for the constraint that there should be equal number
    % number of occurrences of "bits".
    %  
    /\ % converts binary -> bin_code 
    forall(i in 1..m) (
      bin_code[i] = binary[i,1]
    ) 
    /\ % number of occurences in bin_code 
    % global_cardinality_old(bin_code, gcc)
    global_cardinality(bin_code, array1d(0..base-1, [i | i in 0..base-1]), gcc)

   /\  % when m mod base = 0 ->
       %    it should be exactly equal number of occurrences
       % otherwise, it should not (and could not).
       % Note: this may take some time for larger problems.
    if m mod base = 0 then
       forall(i in 1..base-1) (
         gcc[i] = gcc[i-1]
         /\
         gcc[i] = m div base
       )
    else
       true
    endif
;


%
% A lot of information is printed.
%
output [
  if i = 1 then 
    "\nn: "        ++ show(n)        ++ " m: " ++ show(m) ++ " base: " ++ show(base) ++ 
    "\nbin_code: " ++ show(bin_code) ++
    "\ngcc: "      ++ show(gcc)      ++
    "\nbinary code: " 
  else "" endif      ++
   show(bin_code[i]) ++ ""
  | i in 1..m
] 
++
[
 if i = 1 /\ j = 1 then
    "\nx (decimal representation):\n" ++ show(x) 
 else "" endif 
 ++
 if j = 1 then "\n" else " " endif ++
   show(binary[i,j])
 | i in 1..m, j in 1..n
] ++ 
["\n"]
;
