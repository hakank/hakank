/*

  Three jugs problem in SWI Prolog

  Modelled as a shortest path problem.

  Problem from Taha "Introduction to Operations Research", page 245f

  Also see http://mathworld.wolfram.com/ThreeJugProblem.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 15,
        Start = 1, %% start node
        End = 15,  %% end node
        M = 9,     %% a large number
   
        Nodes = [
                 "8,0,0", %% start
                 "5,0,3",
                 "5,3,0",
                 "2,3,3",
                 "2,5,1",
                 "7,0,1",
                 "7,1,0",
                 "4,1,3",
                 "3,5,0",
                 "3,2,3",
                 "6,2,0",
                 "6,0,2",
                 "1,5,2",
                 "1,4,3",
                 "4,4,0" %% goal
      ],

        %% distance matrix (the moves)
        D = [[M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M],
             [M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M],
             [M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M],
             [M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M],
             [M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M],
             [M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M],
             [M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M],
             [M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1], 
             [M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M],
             [M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M],
             [M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M],
             [M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M],
             [M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M],
             [M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1], 
             [M, M, M, M, M, M, M, M, M, M, M, M, M, M, M]],
        
        length(D,N),
   
        %% decision variables
        
        %% the resulting matrix, 1 if connected, 0 else
        new_matrix(N,N,0..1,X),
        flatten(X,XFlatten),
        
        length(OutFlow,N),
        OutFlow ins 0..1,

        length(InFlow,N),
        InFlow ins 0..1,

        length(Rhs,N), %% requirements (right hand statement)
        % Rhs ins -1..1,

        %% objective to minimize
        Z in 0..M,

        %% total cost/length (Z) to minimize
        flatten(D,DFlatten),
        scalar_product(DFlatten,XFlatten,#=,Z),

        numlist(1,N,Is),
        maplist(rhs(Rhs,Start,End),Is),
      
        %% outflow constraint
        maplist(outflow(X,D,N,M,OutFlow),Is),
   
        %% inflow constraint
        maplist(inflow(X,D,N,M,InFlow),Is),
   
        %% inflow = outflow
        maplist(inflow_eq_outflow,InFlow,OutFlow,Rhs),
        
        %% solve
        flatten([OutFlow, InFlow],Vars),

        labeling([min(Z)], Vars),
   
        writeln(z=Z),
        format("InFlow = ~w\n", [InFlow]),
        format("OutFlow= ~w\n", [OutFlow]),
        writeln("Path:"),
        findall(Node,
                (between(1,N,I),
                 element(I,InFlow,1),
                 nth1(I,Nodes,Node)
                ),
                Flow1
               ),
        nth1(Start,Nodes,StartNode),
        append([StartNode],Flow1,Flow),
        maplist(writeln,Flow),
        nl.


rhs(Rhs,Start,_End,Start) :-
        element(Start,Rhs,1).
rhs(Rhs,_Start,End,End) :-
        element(End,Rhs,-1).
rhs(Rhs,Start,End,I) :-
        I #\= Start,
        I #\= End,
        element(I,Rhs,0).

%% outflow constraint
outflow(X,D,N,M,OutFlow,I) :-
        findall(J,
                (between(1,N,J),
                 matrix_element(D,I,J,DIJ),
                 DIJ #< M
                ),
                Js),
        sum_js(Js,I,X,0,Sum),
        element(I,OutFlow,Sum).

sum_js([],_I,_X,Sum,Sum).
sum_js([J|Js],I,X,Sum0,Sum) :-
        matrix_element(X,I,J,XIJ),
        Sum1 #= Sum0 + XIJ,
        sum_js(Js,I,X,Sum1,Sum).


%% inflow constraint
inflow(X,D,N,M,InFlow,J) :-
        findall(I,
                (between(1,N,I),
                 matrix_element(D,I,J,DIJ),
                 DIJ #< M
                ),
                Is),
        sum_is(Is,J,X,0,Sum),
        element(J,InFlow,Sum).

sum_is([],_K,_X,Sum,Sum).
sum_is([I|Is],J,X,Sum0,Sum) :-
        matrix_element(X,I,J,XIJ),
        Sum1 #= Sum0 + XIJ,
        sum_is(Is,J,X,Sum1,Sum).

%% inflow = outflow
%% foreach(I in 1..N) OutFlow[I]-InFlow[I]#=Rhs[I] end,
inflow_eq_outflow(InFlow,OutFlow,Rhs) :-
        OutFlow-InFlow #= Rhs.
        