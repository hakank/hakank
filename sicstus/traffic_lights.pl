/*

  Traffic lights problem in SICStus Prolog.

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
  
  Compare with these models: 
  * MiniZinc: http://www.hakank.org/minizinc/traffic_lights.mzn
  * Comet   : http://www.hakank.org/comet/traffic_lights.co
  * ECLiPSe : http://www.hakank.org/eclipse/traffic_lights.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :- 
        findall([V,P], traffic_lights(V,P), L),
        ( foreach([V,P], L) do
              ( for(I,1,4), param(V,P) do
                    element(I,V,VI),
                    element(I,P,PI),
                    tr(VC,VI),
                    tr(PC,PI),
                    format('~w ~w ',[VC,PC])
              ),
              nl
        ).


traffic_lights(V, P) :-
        N  = 4,
        length(V, N),
        domain(V,1,N),
        length(P, N), 
        domain(P,1,N),

        ( for(I,1,N),
          param(N,V,P) do 
              ( for(J,1,N), 
                param(N,V,P,I) do 
                    JJ is (1+I) mod N,
                    J #= JJ ->
                    element(I,V,VI),
                    element(I,P,PI),
                    element(J,V,VJ),
                    element(J,P,PJ),
                    check_allowed(VI, PI, VJ, PJ)
                    % check_allowed_table(VI, PI, VJ, PJ)
              ; 
                    true
              )
        ),

        append(V,P,Vars),
        labeling([],Vars).


check_allowed(VI, PI, VJ, PJ) :-
        ( foreach(El,[VI, PI, VJ, PJ]),
          fromto(L,[C|In],In,[]) do
              E #= El,
              tr(C,E)
        ),
        allowed(L).

% Ah, table/2 needs integers...
% check_allowed_table(VI, PI, VJ, PJ) :-
%        allowed_table(Table),
%        table([VI, PI, VJ, PJ],Table).

        

tr(r,1).
tr(ry,2).
tr(g,3).
tr(y,4).

allowed([r,r,g,g]).
allowed([ry,r,y,r]). 
allowed([g,g,r,r]).
allowed([y,r,ry,r]).


allowed_table([[r,r,g,g],
               [ry,r,y,r], 
               [g,g,r,r],
               [y,r,ry,r]]).
