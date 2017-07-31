/*

  (Decomposition of) global constraint circuit in ECLiPSe.

  See Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/circuit_test.mzn
  * Gecode  : http://www.hakank.org/gecode/circuit_orbit.cpp


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(listut).
:-lib(propia).

go :-   
        % find all circuits of order 5
        N = 5,
        dim(X,[N]),
        X :: 1..N, 

        term_variables(X,Vars),
        findall([X,backtracks:Backtracks], (circuit(X),search(Vars,0,first_fail,indomain_min,
                                      complete,[backtrack(Backtracks)])),L),
        length(L,Len),
        writeln(L),
        writeln(len:Len),
        ( foreach(LL, L) do
              writeln(LL)
        ).

%
% show all circuits of order 8
%
go2 :-
        N = 8,
        dim(X,[N]),
        X :: 1..N,
        circuit(X),

        term_variables(X,Vars),
        search(Vars,0,first_fail,indomain_min,complete,[backtrack(Backtracks)]),
        writeln(X),
        writeln(backtracks:Backtracks),fail.
        
       
%
% circuit(X) succeeds for the array X if it's a circuit.
%
% This implementation use an extra array (Z) for the orbit of x[1].
%
circuit(X) :-

        dim(X,[N]),
        dim(Z,[N]),
        Z :: 1..N,

        %
        % The main constraint is that Z[I] must not be 1 
        % until I = N, and for I = N it must be 1.
        %

        ic_global:alldifferent(X),
        ic_global:alldifferent(Z),

        %
        % put the orbit of x[1] in in z[1..n]
        %
        X[1] #= Z[1],
        
        %
        % when i = n it must be 1
        %
        Z[N] #= 1,


        %
        % Get the orbit for Z.
        %
        % The MiniZinc code for the following is:
        %    forall(i in 2..n) (
        %       z[i] = x[z[i-1]]
        %    )
        % 
        % but since ECLiPSe don't support this syntax we have to 
        % twiddle a little. 

        %
        % The first approach is using an array of reifications.
        % I use suspend below but let this code stand as a 
        % reminder.
        %
        % (for(I,2,N), param(X,Z,N) do
        %     ZI1 #= Z[I-1],
        %     ( for(J,1,N), 
        %       fromto(Reif,Out,In,[]), 
        %       param(X,Z,ZI1,I) do
        %           T #= (ZI1 #= J and Z[I] #= X[J]),
        %           Out = [T|In]
        %     ),
        %     sum(Reif) #> 0
        %),

        % Using suspend is much nicer.
        (for(I,2,N), param(X,Z) do
             ZI1 #= Z[I-1],
             % suspend(Z[I] #= X[Z[I-1]], 2, ZI1->inst)
             Z[I] #= X[Z[I-1]]
        ),


        % Redundant constraint. It is covered by the constraints
        % X[N] = 1 and alldifferent.
        % 
        % should not be 1 for i < n
        (for(I,1,N-1), param(Z) do
             Z[I] #\= 1
        ).
