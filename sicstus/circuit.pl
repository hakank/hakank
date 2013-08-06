/*

  Decomposition of) global constraint circuit in SICStus Prolog.

  See Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/circuit_test.mzn
  * Gecode  : http://www.hakank.org/gecode/circuit_orbit.cpp
  * ECLiPSe : http://www.hakank.org/eclipse/circuit.ecl


  Note: SICStus Prolog has a built in circuit/1 so this model
        should really be considered an etude.

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-   
        % find all circuits of order 8
        N = 8,
        length(X,N),
        domain(X, 1,N), 

        findall(X, (my_circuit(X), labeling([ff],X)),L),
        length(L,Len),
        write(L),nl,
        write(len:Len),nl,
        fd_statistics.

go_b :-   
        % using SICStus' circuit/1
        N = 8,
        length(X,N),
        domain(X, 1,N), 

        findall(X, (circuit(X), labeling([],X)),L),
        length(L,Len),
        write(L),nl,
        write(len:Len),nl,
        fd_statistics.


%
% show all circuits of order 8 (by fail)
%
go2 :-
        N = 118,
        length(X,N),
        domain(X, 1, N),
        my_circuit(X),

        labeling([],X),
        write(X),nl,fail.
        
       
%
% circuit(X) succeeds for the array X if it's a circuit.
%
% This implementation use an extra array (Z) for the orbit of x[1].
%
my_circuit(X) :-

        length(X,N),
        length(Z,N),
        domain(Z,1,N),

        all_different(X),
        all_different(Z),

        % put the orbit of x[1] in in z[1..n]
        element(1,X,X1),
        element(1,Z,Z1),
        X1 #= Z1, 

        % when I = N it must be 1
        element(N,Z,1),

        %
        % The main constraint is that Z[I] must not be 1 
        % until I = N, and for I = N it must be 1.
        %
        % Get the orbit for Z.
        %
        ( for(I,2,N), 
          param(X,Z) do
              % Z[I] #= X[Z[I-1]] % ECLiPSe code
              I1 is I-1,
              element(I1,Z,ZI1),
              element(I,Z,ZI),
              element(ZI1,X,XZI1),
              ZI #= XZI1
        ).

        % Redundant constraint:
        % Z[I] should not be 1 for I < n
        % 
        % It is already covered by the constraints
        % X[N] = 1 and alldifferent.
        %
        % It seems to do no good (but no harm either).
        % 
        % ( foreacharg(A,Z,Ix) 
        % do 
        %   Ix \= N 
        % -> 
        %   A #\= 1 
        %;
        %   true
        % ).


end_of_file.

Some comparison between my_circuit/1 and the
built in circuit/1:

my_circuit/1 for order 8, with all_different/1
  Resumptions: 56243
  Entailments: 23796
  Prunings: 49794
  Backtracks: 0
  Constraints created: 26

               order 10, with all_different/1
  Time: 25.5s
  Resumptions: 4047899
  Entailments: 1712190
  Prunings: 3582804
  Backtracks: 0
  Constraints created: 32

               order 10, with all_different(X, [consistency(domain)])
  Time: 31.2s
  Resumptions: 5499419
  Entailments: 2437950
  Prunings: 3582804
  Backtracks: 0
  Constraints created: 34


circuit/1 for order 8:
  Resumptions: 37479
  Entailments: 15120
  Prunings: 27406
  Backtracks: 0
  Constraints created: 3

              order 10
  Time: 9.6s
  Resumptions: 2698579
  Entailments: 1088640
  Prunings: 1972828
  Backtracks: 0
  Constraints created: 3
