/*

  Talisman Square in ECLiPSe.
 
  http://mathworld.wolfram.com/TalismanSquare.html
  """
  An n×n array  of the integers from 1 to n^2 such that the difference between 
  any one integer and its neighbor (horizontally, vertically, or 
  diagonally, without wrapping around) is greater than or equal to
  some value k is called a n,k)-talisman square. 
  """

  Compare with the MiniZinc model:
  http://www.hakank.org/minizinc/talisman_square.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
:-lib(propia).


go :-
        N = 5,
        K = 4,
        talisman_square(N, K, X),
        pretty_print(X),
        fail.

talisman_square(N,K,X) :-

        N2 is N*N,
        dim(X,[N,N]),
        X :: 1..N2, 

        alldifferent(X),

        ( for(I,2,N) * for(J,2,N), param(X,K) do
              abs(X[I,J]-X[I-1,J]) #>= K,
              abs(X[I,J]-X[I,J-1]) #>= K
        ),

        ( for(I,1,N-1) * for(J,1,N-1), param(X,K) do
              abs(X[I,J]-X[I+1,J]) #>= K,
              abs(X[I,J]-X[I,J+1]) #>= K
        ),



        % some symmetry breaking
        X[1,1] #= 1,

        labeling(X).


pretty_print(X) :-
        dim(X, [N,N]),
        ( for(I, 1, N), param(X, N) do
            ( for(J, 1, N), param(X, I) do
                XX is X[I,J],
                printf("%2d", XX),
                write(" ")
            ),
            nl
        ),nl.        
