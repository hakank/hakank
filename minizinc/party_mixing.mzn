% 
% Party mixing in MiniZinc.
%
%  Based on a real problem:
%
%  27 people sits at 4 tables and shall be mixed in some way.
%  What is the optimal mixing so a person will talk with as 
%  many different people as possible? (Thanks Simon!)
%
% [This is a MiniZinc'ed version of my Picat model:
%  http://hakank.org/picat/party_mixing.pi 
%
%  only predicate party_mixing/4 is translated
% ]
%
%  Here are some assumptions:
%
%  - There are NumTables tables and NumAtTables chairs at each table
%
%  - There is exactly one _empty chair_ which is the chair that is used in
%    each move: each person move to the empty chair, and thus
%    a new chair will be available (and is the new empty chair).
%
%    The number of moves is thus NumTables*NumAtTables - 1.
%
%  - The main constraint is that a person must move from one table
%    to another table. After all people has moved, no one will sit at 
%    his/her original table.
%
%    However, we don't care about the exact position at the table, 
%    i.e. thereis no constraint that forbids that to former neighbours 
%    from table 1 will sit together at table 4.
%    Note: this might be implemented in a forthcoming model.
%
%    For more than 2 tables we requires that the new immediate 
%    neighbours (to the left and right) is not from the same old table.
%
%  - The model use random heuristics (in solve/1) to make the moves 
%    more random looking.
%
%
%  Exactly one free chair and possible decompositions
%  ---------------------------------------------------
%  A note regarding the assumption of one free chair.
%
%  Both models assumes that there is exactly one free chair at any move.
%  This mean that we will use N+1 chairs. However, the total number of people 
%  (N+1) may not be decomposed exactly in NumTables * NumAtTables. 
%
%  As shown in go4/0, all N > 2 where N+1 is not a prime number 
%  are decomposable in this manner, often in more than one ways. 
%
%  Examples:
%  - N = 27
%    2 tables x 14 chairs -1 = 27
%    4 tables x  7 chairs -1 = 27
%    7 tables x  4 chairs -1 = 27
%    14 tables x 2 chairs -1 = 27

%  - N = 35
%    Tables Chairs
%      2    18
%      3    12
%      4     9
%      6     6
%      9     4
%     12     3
%     18     2
%
%  For N+1 is a prime there is no decomposition. 
%  What we can do the  is to add one chair - using a dummy person - which is 
%  used in the same way as all the other people/chairs. The move for this person
%  will be an empty move which may be immediately followed by a real move.
%
%  N+1 is a prime:
%  - N = 28
%    28 is a prime - 1. Hence we use N = N+1 = 29 instead.
%  
%    Tables Chairs
%       2   15
%       3   10
%       5    6
%       6    5
%      10    3
%      15    2
%
%  Another way of fixing this is to let one person, e.g. the host,
%  be outside the moving and use N chairs (with one empty chairs) as planned.
%

% Benchmark:
%
% Only testing solvers that has a built-in circuit/1 constraint:
%
%   - Gecode: occurrence/indomain_random
%   - JaCoP: input_order/indomain_random
%   - Picat CP: input_order/indomain_random
%   - Picat SAT: input_order/indomain_random
%   - SICStus: input_order/indomain_random
%   - (Mistral)
% 
% The time is total, including flattening etc.
%
%   Tables Chairs  #chairs  Gecode    Picat CP  Picat SAT  JaCoP    SISCtus
% ----------------------------------------------------------------------------------------------
%    4       7      28       0.88s     0.135s    2.030s    0.383s  0.411s
%   10       7      70       0.127s    0.156s   17.169s    0.497s  0.438s
%   14      17     238       0.297s    0.321s   >2min      1.006s  1.517s
%   24      10     240       0.308s    0.320s   -          0.983s  1.525s
%    2(*)  300     600       1.144s    3.938s   -          1.891s  5.209s
%  300       2     600       2.354s    2.328s   -          5.558s  21.276s
%   24      27     648       3.080s    2.760s   -          5.965s  21.271s
%   40      30    1200      19.588s   16.220s   -          >7min   RAM exh.
%   40      40    1600       >4min    43.952s   -          -       -
%  500       4    2000       -       107.53s    -          -       -
%
%  (*) not requiring that neighbours should be different  
%
% 
% Presentation
% ------------
% For presentation, circuit_path/2 can be used but this takes much
% longer so it's skipped in the benchmark.
%
% Here's the presentation (using circuit_path/2) for the 4 x 7 instance: 
% """
% Start:
%
%   1   2   3   4   5   6   7
%   8   9  10  11  12  13  14
%  15  16  17  18  19  20  21
%  22  23  24  25  26  27  28
%
% End: 
%  23  20  10  17  28  12  25
%   7  16  27  18   3  24  19
%  22   4  13   5  11  26   6
%   2   8   1  21  14  15   9
%
% End (tables):
%
%  4   3   2   3   4   2   4
%  1   3   4   3   1   4   3
%  4   1   2   1   2   4   1
%  1   2   1   3   2   3   2
%
% Path: [23, 8, 7, 25, 21, 6, 12, 3, 10, 27, 15, 22, 2, 20, 26, 14, 19, 11, 18, 5, 28, 9, 16, 4, 17, 13, 24, 1]
% 8 -> 23
% 7 -> 8
% 25 -> 7
% 21 -> 25
% """
% 
% I.e. the first move is that person 23 move to chair 1, and then person 8 move to chair 23, etc.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: NumTables = 4;
int: NumAtTables = 7; % number of chairs at each table
int: Total = NumTables*NumAtTables;

% decision variables
array[1..NumTables,1..NumAtTables] of var 1..Total: X;
array[1..Total] of var 1..Total: L = array1d(X);
% array[1..Total] of var 1..Total: Path;
% array[1..NumTables,1..NumAtTables] of var 1..NumTables: Tables = 
%                         array2d(1..NumTables,1..NumAtTables, [(1+((X[I,J]-1)) div NumAtTables) | I in 1..NumTables, J in 1..NumAtTables]);

% solve satisfy;
% solve :: int_search(L, occurrence, indomain_random, complete) satisfy;
solve :: int_search(L, occurrence, indomain_random, complete) satisfy;


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

    % ensure that people that starts at table T don't stays at the same table
    (1+((X[I,J]-1)) div NumAtTables) != (1+(((I-1)*NumAtTables+J)-1) div NumAtTables)
    % /\ Tables[I,J] != (1+(((I-1)*NumAtTables+J)-1) div NumAtTables)
    /\

    % Ensure that two neighbours from the same table will not sit beside each other
    % in the new configuration.
    % Note that for N <= 2 this will give no solution, so the constraint is ignored.
    if NumTables > 2 then
      if J > 1 then
        1+(X[I,J-1]-1) div NumAtTables != 1+(X[I,J]-1) div NumAtTables
        % /\ Tables[I,J-1] != Tables[I,J]
      else 
        true
      endif
      /\
      if J < NumAtTables then
        1+(X[I,J+1]-1) div NumAtTables != 1+(X[I,J]-1) div NumAtTables
        % /\ Tables[I,J+1] != Tables[I,J]
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
  "Total: \(Total)\n",
  "Start:\n",
]
++
[
 if J = 1 then "\n" else " " endif ++
   show_int(3, (I-1)*NumAtTables + J)

 | I in 1..NumTables, J in 1..NumAtTables
]
++ ["\n\nEnd: "] ++
[
 if J = 1 then "\n" else " " endif ++
   show_int(3,X[I,J])

 | I in 1..NumTables, J in 1..NumAtTables
]
% ++
% ["\n\nEnd (tables):\n"]
% ++
% [
%  if J = 1 then "\n" else " " endif ++
%    show_int(3,Tables[I,J])
%  | I in 1..NumTables, J in 1..NumAtTables
% ]
% ++ ["\n\nMove order:\n"] 
% ++ ["\n\nPath: \(Path)\n"]
% ++ [ "\n\(Path[1]) -> 1\n" ] 
% ++
% [
%   "\(Path[K]) -> \(Path[K-1])\n"
%   | K in 2..Total
% ]
;

