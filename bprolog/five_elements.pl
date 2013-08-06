/*

  Five elements problem in B-Prolog.

  From 
  Charles W. Trigg, PI MU EPSILON JOURNAL, Valume 6, Fall 1977, Number 5
  """
  From the following square array of the first 25 positive integers, 
  choose five, no two of the same row or column, so that the maximum of
  the five elements is as small as possible.

     2 13 16 11 23
    15  1  9  7 10
    14 12 21 24  8
     3 25 22 18  4
    20 19  6  5 17
  """
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        Matrix = [[ 2, 13, 16, 11, 23],
                  [15,  1,  9,  7, 10],
                  [14, 12, 21, 24,  8],
                  [ 3, 25, 22, 18,  4],
                  [20, 19,  6,  5, 17]],
        N = 5,

        %
        % decision variables
        %
        new_array(X,[N,N]),
        term_variables(X,XVar),
        XVar :: 0..1,
        length(Y, N),
        Y :: 1..N*N,

        %
        % constraints
        %

        % ensure unicity of rows and columns
        foreach(I in 1..N, 
                (sum([X[I,J] : J in 1..N]) #= 1,
                 sum([X[J,I] : J in 1..N]) #= 1
                )),

        % Find the specific row I for which X[I,J] is 1.
        %
        % Using freeze/2 and using the Is list in the labeling 
        % seems to be the only way (when using this matrix approach)
        foreach(J in 1..N, [I],ac(Is,[]),
                (I :: 1..N,
                 freeze(I, 1 #= X[I,J]),
                 freeze(I, Y[J] #= Matrix[I,J]),

                 % It would be nice is we can do simply this,
                 % i.e. without the freeze:
                 %   1 #= X[I,J],
                 %   Y[J] #= Matrix[I,J],

                 Is^1 = [I|Is^0])),

        MaxY #= max(Y),

        % search
        term_variables([XVar,Y,MaxY,Is], Vars),
        minof(labeling([ff], Vars),MaxY),
        labeling(Vars),

        writeln(y:Y),
        writeln(maxY:MaxY),
        Rows @= X^rows,
        foreach(Row in Rows, writeln(Row)),

        nl.
