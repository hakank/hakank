% 
% Party mixing in MiniZinc.
%
% Based on a real problem:
%
% 27 people sits at 4 tables and shall be mixed in some way.
% What is the optimal mixing so a person will talk with as 
% many different people as possible? (Thanks Simon!)
%
% This is a simpler version of 
%   http://hakank.org/minizinc/party_mixing.mzn
% where the matrix is ditched and only the array L (the circuit)
% is used.
% (The array Path is the path of the moves, but adding it takes
% longer time.)
% The time seems to be the same as party_mixing.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: NumTables = 24;
int: NumAtTables = 27; % number of chairs at each table
int: Total = NumTables*NumAtTables;

% decision variables
array[1..Total] of var 1..Total: L;
% array[1..Total] of var 1..Total: Path;

% solve satisfy;
% solve :: int_search(L, occurrence, indomain_random, complete) satisfy;
solve :: int_search(L, input_order, indomain_random, complete) satisfy;


%
% Extract the path from the circuit.
% Assumption: both x and p are stated as all different (or circuit)
%
predicate circuit_path(array[int] of var int: x, 
                       array[int] of var int: p) =
   let {
     int: len = length(x)
   } in
   % we always starts the path at 1
   p[1] = x[1]
   /\
   p[len] = 1
   /\
   forall(i in 2..len) (
     p[i] = x[p[i-1]] 
   )
; 


constraint

  forall(I in 1..NumTables, J in 1..NumAtTables) (
    let {
      int: K = (I-1)*NumAtTables + J
    } in 
    % ensure that people that starts at table T don't stays at the same table
    (1+((L[K]-1)) div NumAtTables) != (1+(((I-1)*NumAtTables+J)-1) div NumAtTables)
    /\

    % Ensure that two neighbours from the same table will not sit beside each other
    % in the new configuration.
    % Note that for N <= 2 this will give no solution, so the constraint is ignored.
    if NumTables > 2 then
      if J > 1 then
        1+(L[K-1]-1) div NumAtTables != 1+(L[K]-1) div NumAtTables
      else 
        true
      endif
      /\
      if J < NumAtTables then
        1+(L[K+1]-1) div NumAtTables != 1+(L[K]-1) div NumAtTables
      else 
        true
      endif
    else
      true
    endif

  )


  % There should be no sub circuits (sub loops).
  % (It also constrains the values to be different.)
  /\ circuit(L)
  % /\ all_different(L)
  % /\ all_different(Path)
  % /\ circuit_path(L,Path)
;


output 
[
  "L: \(L)\n",
  % "Path: \(Path)\n",
]
++ ["Matrix:\n"] ++
[
  if J = 1 then "\n" else " " endif ++
    show_int(3,L[(I-1)*NumAtTables + J])
  | I in 1..NumTables, J in 1..NumAtTables
]
++
["\n"]
;

