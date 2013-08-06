/*

  Knights path problem in SICStus Prolog.

  
  Create a knights path in a n x n matrix for all integers from 1..n*n-1.
  The integer n*n is placed whatever it may fit...

  
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/knight_path.mzn
  * Comet   : http://www.hakank.org/comet/knights_path.co
  * ECLiPSe : http://www.hakank.org/eclipse/knights_path.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 5,
        knights_path(N, X),
        pretty_print(X), 
        nl, nl,fd_statistics,
        fail.

knights_path(N, X) :-
        
        N2 is N*N,
        matrix(X,[N,N]),
        append(X,XList),
        domain(XList,1,N2),

        all_different(XList),

        % some symmetry breaking: start with 1 in upper left
        % matrix_element(X,1,1,1),

        %
        %  place all integers 1..n^2
        %
        % Note: This is the same principle as the loop in
        % http://www.hakank.org/sicstus/hidato.pl
        % 
        % NN2 is N*N-1,
        NN2 is N*N-1,
        C = [-2,-1,1,2],
        list_to_fdset(C,CSet),
        ( for(K,1,NN2),
          param(XList,CSet,N) do

              % define temporary variables for finding
              % the index of this and the next number (K)
              I in 1..N, % index I
              J in 1..N, % index J
              A in_set CSet, % offset from K's position
              B in_set CSet, % ibid

              % needed for nth1
              IA #= I+A,
              JB #= J+B,
              
              % some extra constraints
              IA #>= 1,
              JB #>= 1,
              IA #=< N,
              JB #=< N,
              abs(A)+abs(B) #= 3, % it must be a knight move, i.e. 2+1 = 3
              
              % 1) First: fix this k, i.e.
              IJ #= (I-1)*N + J,
              element(IJ, XList, K),
              
              % 2) Then, find the position of the next value, i.e.
              IA_JB #= (I-1+A)*N + J+B,
              K1 is K+1,
              element(IA_JB, XList, K1),

              indomain(I), indomain(J),
              indomain(A), indomain(B)

        ),

        labeling([ffc,bisect,down],XList).


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).

pretty_print(X) :-
        ( foreach(Row,X) do
              ( foreach(R,Row) do
                    format('~w\t',[R])
              ),
              nl
        ).

% moves(-1,-2).
% moves(-1,2).
% moves(1,-2).
% moves(1,2).
% moves(-2,-1).
% moves(-2,1).
% moves(2,-1).
% moves(2,1).
