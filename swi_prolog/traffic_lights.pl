/*

  Traffic lights problem in SWI Prolog.

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :- 
        % symbol version of the allowed light combinations
        Allowed1 = [[r,r,g,g],
                    [ry,r,y,r],
                    [g,g,r,r],
                    [y,r,ry,r]],
        traffic_lights_table(V,P,Allowed1),
        print_result(V,P),
        nl.


%
% Using table Allowed
%
traffic_lights_table(V, P, Allowed1) :-
        N = 4,

        % Convert to integers according to tr/2.
        % Note: table_in requires structure as a term of the format: (...)
        findall(DD, (member(A,Allowed1),
                     findall(D, (member(S,A), tr(S,D)),DD)
                   ),
                Allowed),
        length(V, N),
        V ins 1..N,
        length(P,N),
        P ins 1..N,

        %% Indices
        findall([I,J],
               (between(1,N,I),between(1,N,J), J #= (1+I) mod N),
               IJs
              ),
        tuples_loop(IJs,V,P,Allowed),
        append(V,P,Vars),
        labeling([],Vars).

tuples_loop([],_V,_P,_Allowed).
tuples_loop([[I,J]|IJs],V,P,Allowed) :-
        nth1(I,V,VI),nth1(I,P,PI),
        nth1(J,V,VJ),nth1(J,P,PJ),
        %% Note: The Tuples list should be a list of a list
        %% (i.e. not just a list).
        tuples_in([[VI, PI, VJ, PJ]],Allowed),
        tuples_loop(IJs,V,P,Allowed).

%%
%%% translation table of symbols <-> integer
%%
tr(r, 1).
tr(ry,2).
tr(g, 3).
tr(y, 4).

%%
%% Translate a number -> symbolic representation
%%
print_result(V,P) :-
        zip2(V,P,Zipped),
        flatten(Zipped,Flatten),
        findall(T,(member(F,Flatten),tr(T,F)),Result),
        writeln(Flatten),
        writeln(Result).
