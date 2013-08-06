/*

  Lectures problem in SICStus Prolog.

  Biggs: Discrete Mathematics (2nd ed), page 187.
  """   
  Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
  Among the the potential audience there are people who wish to hear both
 
   - v1 and v2
   - v1 and v4
   - v3 and v5
   - v2 and v6
   - v4 and v5
   - v5 and v6
   - v1 and v6
 
  How many hours are necessary in order that the lectures can be given
  without clashes?
  """    

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/lectures.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 6,      % number of nodes
        g(Graph),

        MaxC in 1..N,
        length(V, N),
        domain(V, 1,N),
        maximum(MaxC, V),

        ( foreach([L1,L2], Graph),
          param(V) do
              element(L1,V,V1),
              element(L2,V,V2),
              V1 #\= V2
        ),

        % symmetry breaking: 
        % v1 has the color 1, v2 has either color 1 or 2
        % (this should be enough for a general model)
        element(1,V,1),
        element(2,V,V2),
        V2 #=< 2,

        labeling([minimize(MaxC)], V),
        
        write(v:V),nl,
        write(max_c:MaxC),nl,
        fd_statistics.



% The schedule requirements:
%     lecture a cannot be held at the same time as b
g([
   [1, 2],
   [1, 4],
   [3, 5],
   [2, 6],
   [4, 5],
   [5, 6],
   [1, 6]]).
