/*

  (Decomposition of) global constraint circuit in B-Prolog.

  See Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

%
% Reporting both time and backtracks
%
time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).

        
go :-   
        % find all circuits of order 5
        N = 5,
        Print = 1,
        test1(N, Print),
        test2(N, Print).


%
% Show all circuits of order 4..10 and compare with
% the built-in circuit/1 which is (unsurprisingly)
% much faster. Intrestingly the backtracks is the same.
% For N=10
%  - circuit_me/1: 3.388 seconds. Backtracks: 362879
%  - circuit/1   : 2.144 seconds. Backtracks: 362879
% (There are 362880 different solutions of N=10,
%  i.e. one more than the number of backtracks).
%
% For N=11:
% - circuit_me/1: 36.062 seconds. Backtracks: 3628799
% - circuit/1   : 23.374 seconds. Backtracks: 3628799
% 
go2 :-
        Print = 0,
        foreach(N in 4..11,
                (
                    writeln('\nN':N),
                    test1(N, Print),
                    test2(N, Print)
                )).


% Using my circuit_me/1
test1(N,Print) :-
        length(X,N),
        X :: 1..N, 
        time2(findall(X, (circuit_me(X),labeling([ff],X)),L)),
        length(L,Len),
        (Print =:= 1 -> writeln(L) ; true),
        writeln(len:Len).


% Using the built-in circuit/1.
test2(N,Print) :-
        length(X,N),
        X :: 1..N, 
        time2(findall(X, (circuit(X),labeling([ff],X)),L)),
        length(L,Len),
        (Print =:= 1 -> writeln(L) ; true),
        writeln(len:Len).

        
       
%
% circuit(X) succeeds for the array X if it's a circuit.
%
% This implementation use an extra array (Z) for the orbit of x[1].
%
circuit_me(X) :-

        length(X,N),
        length(Z,N),
        Z :: 1..N,

        %
        % The main constraint is that Z[I] must not be 1 
        % until I = N, and for I = N it must be 1.
        %
        alldifferent(X),
        alldifferent(Z),

        % put the orbit of x[1] in in z[1..n]
        X[1] #= Z[1],
        
        % when I = N it must be 1
        Z[N] #= 1,

        %
        % Get the orbit for Z.
        %
        % The MiniZinc code for the following is:
        %    forall(i in 2..n) (
        %       z[i] = x[z[i-1]]
        %    )
        % 
        % but since B-Prolog don't support this syntax we have to 
        % twiddle a little. 

        % Using freeze/2 don't work (probably since I don't understand 
        % how to use it).
        foreach(I in 2..N,[Z1,X2],
                (
                    % Z[I] #= X[Z[I-1]] % this is not supported
                    % freeze(Z,Z[I] #= Z[I-1]),

                    Z1 @= Z[I-1],
                    element(Z1,X,X2),
                    Z[I] #= X2
                    )
               ),

        % Redundant constraint. It is covered by the constraints
        % X[N] = 1 and alldifferent.
        % foreach(I in 1..N-1, Z[I] #\= 1 ),

        Z[N] #= 1.


