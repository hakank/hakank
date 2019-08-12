/*

  Talisman square in SWI Prolog

  http://mathworld.wolfram.com/TalismanSquare.html
  """
  An nXn array of the integers from 1 to n^2 such that the difference between 
  any one integer and its neighbor (horizontally, vertically, or 
  diagonally, without wrapping around) is greater than or equal to
  some value k is called a (n,k)-talisman square. 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   N = 5,
   K = 4,
   talisman_square(N, K, X),
   maplist(writeln,X),
   nl,
   fail.

go.

talisman_square(N,K,X) :-

        NN #= N*N, 
        new_matrix(N,N,1..NN,X),

        flatten(X,Vars),
        all_different(Vars),

        findall([I,J],
                (between(2,N,I),
                 between(2,N,J)
                ),
                IJs1),
        maplist(talisman1(X,K),IJs1),
        
        N1 #= N-1,
        findall([I,J],
                (between(1,N1,I),
                 between(1,N1,J)
                ),
                IJs2),
        maplist(talisman2(X,K),IJs2),
        
        %% symmetry breaking
        matrix_element(X,1,1,1),

        label(Vars).


talisman1(X,K,[I,J]) :-
        matrix_element(X,I,J,XIJ),
        I1 #= I-1,
        matrix_element(X,I1,J,XI1J),
        abs(XIJ-XI1J) #>= K,
        J1 #= J-1,
        matrix_element(X,I,J1,XIJ1),
        abs(XIJ-XIJ1) #>= K.


talisman2(X,K,[I,J]) :-
        matrix_element(X,I,J,XIJ),
        I1 #= I+1,
        matrix_element(X,I1,J,XI1J),
        abs(XIJ-XI1J) #>= K,
        J1 #= J+1,
        matrix_element(X,I,J1,XIJ1),
        abs(XIJ-XIJ1) #>= K.

