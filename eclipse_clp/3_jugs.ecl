/*

  Three jugs problem in ECLiPSe..

  Modelled as a shortest path problem, using the MIP solver eplex.

  Problem from Taha "Introduction to Operations Research", page 245f

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/3_jugs.mzn
  * Comet   : http://www.hakank.org/comet/3_jugs.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(eplex).

go :-

        eplex_solver_setup(min(Z)),

        Start = 1, % start node
        End = 15,  % end node
        M = 999, % large number
        
        Nodes = [](
                  "8,0,0", % start
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
                  "4,4,0" % goal!
                  ),


        %
        % distance matrix
        %
        D = []([](M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M),
               [](M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M),
               [](M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M),
               [](M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M),
               [](M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M),
               [](M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M),
               [](M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M),
               [](M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1), 
               [](M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M),
               [](M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M),
               [](M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M),
               [](M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M),
               [](M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M),
               [](M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1), 
               [](M, M, M, M, M, M, M, M, M, M, M, M, M, M, M)),

        dim(D,[N,N]),

        dim(Rhs, [N]), % requirements (right hand statement)
        collection_to_list(Rhs,RhsList),
        RhsList $:: -1..1,

        % objective to minimize
        Z $:: 0..M,

        %
        % the resulting matrix, 1 if connected, 0 else
        %
        dim(X,[N,N]),
        % Convert to list in order to set the domain.
        collection_to_list(X,XList),
        XList $:: 0..1,

        dim(OutFlow,[N]),
        collection_to_list(OutFlow,OutFlowList),
        OutFlowList $:: 0..1,

        dim(InFlow,[N]),
        collection_to_list(InFlow,InFlowList),
        InFlowList $:: 0..1,
        
        % Z, sum of nodes (objective to minimize)
        ( for(I,1,N) * for(J,1,N),
          fromto(0,In,Out,ZSum),
          param(D,X,M) do
              D[I,J] < M -> 
              Out = In + D[I,J]*X[I,J]
        ;
              Out = In
        ),
        Z $= eval(ZSum),

        % pick start end end nodes
        ( for(I,1,N),
          param(Start,End,Rhs) do
              I == Start -> Rhs[I] $= 1
              ; I == End -> Rhs[I] $= -1
              ; Rhs[I] $= 0
        ),

        % outflow constraint
        (for(I,1,N),
         param(OutFlow,D,X,N,M) do
             ( for(J,1,N),
               fromto(0,In,Out,Sum),
               param(D,X,I,M) do
                   D[I,J] < M -> 
                   Out = In + X[I,J]
             ;
                   Out = In
             ),
             OutFlow[I] $= eval(Sum)
        ),

        %
        % inflow constraint
        %
        (for(J,1,N),
         param(InFlow,D,X,N,M) do
             ( for(I,1,N),
               fromto(0,In,Out,Sum),
               param(D,X,J,M) do
                   D[I,J] < M ->
                   Out = In + X[I,J]
             ;
                   Out = In
             ),
             InFlow[J] $= eval(Sum)
        ),

        %
        % inflow = outflow
        %
        ( for(I,1,N),
          param(Rhs,OutFlow,InFlow) do
              OutFlow[I] - InFlow[I] $= Rhs[I]
        ),

        %
        % solve
        %
        eplex_solve(Z),
        eplex_get(vars, Vars),
        eplex_get(typed_solution, Val),
        Vars = Val, 

        writeln("Moves:"),
        ( for(I,1,N),
          param(N,X,Nodes) do
              ( for(J,1,N),
                param(I,X,Nodes) do
                    XIJ is fix(X[I,J]),
                    XIJ > 0 
              ->
                NodesI is Nodes[I],
                writeln(NodesI)
              ; 
                true
              )
        ),
        writeln(z:Z).
