% 
% de Bruijn sequence with no direct repetition in MiniZinc
%
% From gamedev.net:
% http://www.gamedev.net/topic/584672-de-bruijn-sequence-with-no-direct-repetition-/
% """
% Hello,
% 
% I'm looking for an algorithm which would generate a De Bruijn-like 
% sequence with no direct repetition : I mean that a letter should not be directly 
% followed by the same letter.
% 
% In my project, I need to generate such a sequence with an alphabet of size 
% k=3 and order n=5.
% 
% Is there such an algorithm ?
% """

% Note: This seems to be also known as Kautz sequences
% http://www.mathworks.com/matlabcentral/fileexchange/31201


% 
% This Minizinc program was written by Hakan Kjellerstrand, hakank@bonetmail.com,
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% for the global constraints 
%   - all_different
%   - global_cardinality
include "globals.mzn"; 


int: base =  3;  % the base to use, i.e. the alphabet 0..base-1
int: n    =  3;  % number of bits to use (n = 4 -> 0..base^n-1 = 0..2^4 -1, i.e. 0..15)
% int: m    = 52;  % the length of the sequence. For "arbitrary" de Bruijn sequences
int: m = ceil(pow(int2float(base), int2float(n)));  % the length of the sequence k^n. Use this for "classic" de Bruijn sequences
array[1..m] of var 0..ceil(pow(int2float(base), int2float(n)))-1: x; 
array[1..m, 1..n] of var 0..base-1: binary;              % binary representation of the numbers
array[1..m] of var 0..base-1: bin_code;                  % the sequence in base-representation

%
% some solve variants:
%
% solve satisfy;
% solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

% Change the parameters in credit and bbs for more results
solve :: int_search(x ++ bin_code, first_fail, indomain_split, credit(5,bbs(1))) satisfy;
% solve :: int_search(x, first_fail, indomain_split, complete) satisfy;


%
% converts a number (s) <-> an array of numbers (t) in the specific base.
%
predicate toNum(array[int] of var 0..base-1: t, var 0..ceil(pow(int2float(base), int2float(n))): s,  float: base) =
          let { int: len = length(t) }
          in
          s = sum(i in 1..len) (
            ceil(pow(base, int2float(len-i))) * t[i]
          )
;


%
% the constraints
%
constraint

    % all number must be different
    all_different(x) 

    % /\ % symmetry breaking: the minimum element should be the first element
    % minimum(x[1], x)

    /\ % converts x <-> binary (for all_different(x) )
    forall(i in 1..m) (
       % toNum( [binary[i,j] | j in 1..n], x[i], int2float(base)) % this should work, but don't
       let {
          array[1..n] of var 0..base-1: t
       }
       in
       toNum(t, x[i], int2float(base)) /\
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
    % /\ % converts binary -> bin_code 
    % forall(i in 1..m) (
    %   bin_code[i] = binary[i,1]
    % ) 

    /\ % and the extra constraint: no repeating numbers
    forall(i in 2..m) (
       bin_code[i-1] != bin_code[i]
    )
    /\ % around the corner
    bin_code[1] != bin_code[m]
;


%
% A lot of information is printed.
%
output [
  if i = 1 then 
    "\nn: "        ++ show(n)        ++ " m: " ++ show(m) ++ " base: " ++ show(base) ++ 
    "\nbin_code: " ++ show(bin_code) ++
    "\nbinary code: " 
  else "" endif      ++
   show(bin_code[i]) ++ ""
  | i in 1..m
] 
++
[
%  if i = 1 /\ j = 1 then
    "\nx (decimal representation):\n" ++ show(x) 
%  else "" endif 
%  ++
%  if j = 1 then "\n" else " " endif ++
%    show(binary[i,j])
%  | i in 1..m, j in 1..n
] ++ 
["\n"]
;
