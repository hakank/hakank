/*

  Lectures problem in ECLiPSe.

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

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/lectures.mzn
  * SICStus: http://www.hakank.org/sicstus/lectures.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(branch_and_bound).
:-lib(listut).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(propia).



go :-
        N = 6,      % number of nodes
        g(Graph),

        MaxC :: 1..N,
        length(V, N),
        V :: 1..N,

        maxlist(V,MaxC),

        ( foreach([L1,L2], Graph),
          param(V) do
              nth1(L1,V,V1),
              nth1(L2,V,V2),
              V1 #\= V2
        ),

        % symmetry breaking: 
        % v1 has the color 1, v2 has either color 1 or 2
        % (this should be enough for a general model)
        nth1(1,V,1),
        nth1(2,V,V2),
        V2 #=< 2,

        minimize(labeling(V),MaxC),
        
        write(v:V),nl,
        write(max_c:MaxC),nl.



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
