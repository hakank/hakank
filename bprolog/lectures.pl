/*

  Lectures problem in B-Prolog.

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


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 6,   % number of lectures (nodes)
        g(Graph),

        MaxC :: 1..N,
        length(V, N),
        V :: 1..N,

        MaxC #= max(V),
        foreach([L1,L2] in Graph, V[L1] #\= V[L2]),

        % symmetry breaking: 
        % v1 has the color 1, v2 has either color 1 or 2
        % (this should be enough for a general model)
        V[1] #= 1,
        V[2] #=< 2,

        minof(labeling(V),MaxC),
        
        write(v:V),nl,
        write(max_c:MaxC),nl.



% The schedule requirements:
%   lecture a cannot be held at the same time as b
g([
   [1, 2],
   [1, 4],
   [3, 5],
   [2, 6],
   [4, 5],
   [5, 6],
   [1, 6]]).
