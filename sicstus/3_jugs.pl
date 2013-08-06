/*

  Three jugs problem in SICStus Prolog.

  Modelled as a shortest path problem.

  Problem from Taha "Introduction to Operations Research", page 245f

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/3_jugs.mzn
  * Comet   : http://www.hakank.org/comet/3_jugs.co
  * ECLiPSe : http://www.hakank.org/eclipse/3_jugs.ecl (eplex model)


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        Start = 1, % start node
        End = 15,  % end node
        M = 999, % large number
        
        Nodes = [
                    '8,0,0', % start
                    '5,0,3',
                    '5,3,0',
                    '2,3,3',
                    '2,5,1',
                    '7,0,1',
                    '7,1,0',
                    '4,1,3',
                    '3,5,0',
                    '3,2,3',
                    '6,2,0',
                    '6,0,2',
                    '1,5,2',
                    '1,4,3',
                    '4,4,0' % goal
                ],

        % distance matrix (the moves)
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

        matrix(D,[N,N]),
        
        length(Rhs, N), % requirements (right hand statement)
        domain(Rhs,-1,1),

        % objective to minimize
        Z in 0..M,

        %
        % the resulting matrix, 1 if connected, 0 else
        %
        matrix(X,[N,N]),
        % Convert to list in order to set the domain.
        append(X,XList),
        domain(XList,0,1),

        length(OutFlow,N),
        domain(OutFlow,0,1),

        length(InFlow,N),
        domain(InFlow,0,1),
        
        % sanity rule: X must be 0 where D[i,j] = M
        ( foreach(XRow,X),
          foreach(DRow,D),
          param(M) do
              ( foreach(XR,XRow),
                foreach(DR,DRow),
                param(M) do
                    DR == M ->
                    XR #= 0
              ;
                    true
              )
        ),

        % inflow and outflow
        transpose(X,XTranspose),
        transpose(D,DTranspose),
        ( foreach(InF,InFlow), % inflow
          foreach(OutF,OutFlow), % outflow
          foreach(DRow,D),
          foreach(DColumn,DTranspose),
          foreach(XRow,X),
          foreach(XColumn,XTranspose),
          param(M) do
              ( foreach(DElR,DRow),
                foreach(DElC,DColumn),
                foreach(XElR,XRow),
                foreach(XElC,XColumn),
                fromto(0,InR,OutR,SumR),
                fromto(0,InC,OutC,SumC),
                param(M) do
                    (
                        % rows: outflow
                        DElR < M ->
                            OutR #= InR + XElR%
                    ;
                            OutR = InR
                    ),
                    (
                        % columns: inflow
                        DElC < M ->
                            OutC #= InC + XElC
                    ;
                            OutC = InC
                    )
              ),
              OutF #= SumR,
              InF #= SumC,
              Out #= In + SumZ
        ),

        % number of moves
        sum(XList,#=,Z),


        % pick start end end nodes
         ( foreach(R1,Rhs),
           count(I1,1,_),
           param(Start,End) 
         do
           I1 == Start -> R1 #= 1
         ; I1 == End -> R1 #= -1
         ; R1 #= 0
         ),
              
        % inflow = outflow, and connect to Rhs
        ( foreach(R2,Rhs),
          foreach(OutF2,OutFlow),
          foreach(InF2,InFlow) do
              OutF2 - InF2 #= R2
        ),

        % solve
        append(XList,OutFlow,Vars1),
        append(Vars1,InFlow,Vars2),
        append(Vars2,Rhs,Vars3),
        append(Vars3,[Z],Vars),
        labeling([ff,bisect,up,minimize(Z)], Vars),

        % output
        write(z:Z),nl,
        write('Moves:'),nl,
        ( for(I,1,N),
          param(N,X,Nodes) do
              ( for(J,1,N),
                param(I,X,Nodes) do
                    matrix_element(X,I,J,XIJ),
                    XIJ > 0 
              ->
                nth1(I,Nodes,NodesI),
                write(NodesI),nl
              ; 
                true
              )
        ),
        fd_statistics.


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).
