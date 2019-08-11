/*

  Lectures problem in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   g(Graph),
   length(Graph,N),   % number of lectures (nodes)

   MaxC in 1..N,
   length(V,N),
   V ins 1..N,

   max_list_clp(V,MaxC),
   maplist(constraint(V),Graph),
   
   % symmetry breaking: 
   % v1 has the color 1, v2 has either color 1 or 2
   % (this should be enough for a general model)
   element(1,V,1),
   element(2,V,V2),
   V2 #=< 2,

   labeling([min(MaxC)], V),
   
   writeln(v=V),
   writeln(max_c=MaxC),
   nl.

constraint(V, [L1,L2]) :-
        element(L1,V,VL1),
        element(L2,V,VL2),        
        VL1 #\= VL2.


% The schedule requirements:
%   lecture a cannot be held at the same time as b
g(Graph) :-
        Graph = [[1, 2],
                 [1, 4],
                 [3, 5],
                 [2, 6],
                 [4, 5],
                 [5, 6],
                 [1, 6]].
