/*

  Traffic lights problem in B-Prolog.

  CSPLib problem 16
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
  """
  Specification:
  Consider a four way traffic junction with eight traffic lights. Four of the traffic 
  lights are for the vehicles and can be represented by the variables V1 to V4 with domains 
  {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are 
  for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.
  
  The constraints on these variables can be modelled by quaternary constraints on 
  (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples 
  {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.
 
  It would be interesting to consider other types of junction (e.g. five roads 
  intersecting) as well as modelling the evolution over time of the traffic light sequence. 
  ...
 
  Results
  Only 2^2 out of the 2^12 possible assignments are solutions.
  
  (V1,P1,V2,P2,V3,P3,V4,P4) = 
     {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
     [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}
 
 
  The problem has relative few constraints, but each is very tight. Local propagation 
  appears to be rather ineffective on this problem.   
  """
 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :- 
        findall([V,P], traffic_lights(V,P), L),
        print_results(L),
        nl,

        writeln('Using table constraint:'),
        findall([V2,P2], traffic_lights_table(V2,P2), L2),
        print_results(L2),
        nl.


print_results(L) :-
        foreach([V,P] in L,
                (foreach(I in 1..4,[VI,PI,VC,PC],
                         (
                             VI @= V[I],
                             PI @= P[I],
                             tr(VC,VI),
                             tr(PC,PI),
                             format("~q ~q ",[VC,PC])
                         )
                        ),
                 nl
                )
               ).


traffic_lights(V, P) :-
        N  = 4,

        length(V, N),
        V :: 1..N,
        length(P, N), 
        P :: 1..N,
        foreach(I in 1..N, J in 1..N,[JJ,VI,PI,VJ,PJ],
                (JJ is (1+I) mod N,
                 J #= JJ ->
                     VI @= V[I], PI @= P[I],
                     VJ @= V[J], PJ @= P[J],
                     check_allowed(VI, PI, VJ, PJ)               
                ; 
                     true
                )

        ),
        term_variables([V,P],Vars),
        labeling(Vars).


check_allowed(VI, PI, VJ, PJ) :-
        foreach(El in [VI, PI, VJ, PJ], ac(L1,[]),[C],
                (tr(C,El), L1^1 = [C|L1^0])
        ),
        reverse(L1,L),
        allowed(L).

%
% Using table Allowed
%
traffic_lights_table(V, P) :-
        N  = 4,

        % allowed/1 as a table (translated)
        Allowed = [(1,1,3,3),
                   (2,1,4,1),
                   (3,3,1,1),
                   (4,1,2,1)],
       
        length(V, N),
        V :: 1..N,
        length(P, N), 
        P :: 1..N,
        foreach(I in 1..N, J in 1..N,[JJ,VI,PI,VJ,PJ],
                (JJ is (1+I) mod N,
                 J #= JJ ->
                     VI @= V[I], PI @= P[I],
                     VJ @= V[J], PJ @= P[J],
                     % Table constraint
                     (VI, PI, VJ, PJ) in Allowed
                ; 
                     true
                )

        ),

        term_variables([V,P],Vars),
        labeling(Vars).


tr(r,1).
tr(ry,2).
tr(g,3).
tr(y,4).

% The allowed combinations
allowed([r,r,g,g]).
allowed([ry,r,y,r]). 
allowed([g,g,r,r]).
allowed([y,r,ry,r]).

