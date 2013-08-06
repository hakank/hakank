%
% All interval problem in MiniZinc.
% 
% Different approaches inspired by
% http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=007
%
% Also see
%   http://www.hakank.org/minizinc/all_interval1.mzn
%   http://www.hakank.org/minizinc/all_interval2.mzn
%   http://www.hakank.org/minizinc/all_interval3.mzn
%   http://www.hakank.org/minizinc/all_interval4.mzn
%   http://www.hakank.org/minizinc/all_interval5.mzn
%   http://www.hakank.org/minizinc/all_interval6.mzn
%

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n= 12;
set of int: classes = 0..n-1;
set of int: differ = 1..n-1;


% Search space: The set of permutations of integer range [0..n-1]
array[classes] of var classes: series;
array[differ] of var differ: differences;

solve satisfy;

constraint
   % C1: Each pitch class occurs exactly once
   forall(i,j in classes where i != j) (
     series[i] != series[j]
   )
   /\
   % C2: Differences between neighbouring notes are all different
   % AUX: Addition of auxiliary predicates
   % Auxiliary predicate stores the interval between pairs of neighbouring notes
   forall(i in 1..n-2) (
       differences[i]=abs(series[i+1] - series[i])
   )
   /\
   forall(i,j in differ where i != j) (
       differences[i] != differences[j]
   )
;

output [
  show(series)
];
