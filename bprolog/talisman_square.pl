/*

  Talisman Square in B-Prolog.

  http://mathworld.wolfram.com/TalismanSquare.html
  """
  An nXn array of the integers from 1 to n^2 such that the difference between 
  any one integer and its neighbor (horizontally, vertically, or 
  diagonally, without wrapping around) is greater than or equal to
  some value k is called a n,k)-talisman square. 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N = 5,
        K = 4,
        talisman_square(N, K, X),
        pretty_print(X),
        fail.

talisman_square(N,K,X) :-

        new_array(X,[N,N]),
        array_to_list(X,Vars),
        Vars :: 1..N*N, 

        all_different(Vars),

        foreach(I in 2..N, J in 2..N, 
                [I1,J1],
                (
                    I1 is I-1,
                    J1 is J-1,
                    abs(X[I,J]-X[I1,J]) #>= K,
                    abs(X[I,J]-X[I,J1]) #>= K
              )
        ),

        foreach(I in 1..N-1,J in 1..N-1, 
                [I1,J1],
                (
                    I1 is I+1,
                    J1 is J+1,
                    abs(X[I,J]-X[I1,J]) #>= K,
                    abs(X[I,J]-X[I,J1]) #>= K
              )
        ),

        % some symmetry breaking
        X[1,1] #= 1,

        labeling([], Vars).


pretty_print(X) :-
        Rows @= X^rows,
        foreach(Row in Rows, 
                (
                    foreach(R in Row,format("~2d ",[R])),
                    nl
                )
               ),
        nl.


