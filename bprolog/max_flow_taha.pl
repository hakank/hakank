/*

  Max flow problem in B-Prolog.

  From http://taha.ineg.uark.edu/maxflo.txt
  Taha "Introduction to Operations Research", Example 6.4-2)

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 5,
        Start = 1,
        End = 5,
        C = [[0, 20, 30, 10,  0],
             [0,  0, 40,  0, 30],
             [0,  0,  0, 10, 20],
             [0,  0,  5,  0, 20],
             [0,  0,  0,  0,  0]],
        flatten(C,CC),
        Min #= min(CC),
        Max #= max(CC),
        
        new_array(X, [N,N]),
        array_to_list(X, XVars),
        XVars :: Min..Max,

        length(OutFlow, N),
        length(InFlow, N),

        Total #= sum([X[Start,J] : J in 1..N, C[Start,J] > 0]),

        foreach(I in 1..N, J in 1..N, (X[I,J] #>= 0, X[I,J] #=< C[I,J] )),
        foreach(I in 1..N, InFlow[I] #= sum([X[J,I] : J in 1..N, C[J,I] >0])),
        foreach(I in 1..N, OutFlow[I] #= sum([X[I,J] : J in 1..N, C[I,J]>0])),
        foreach(I in 1..N,
                (I \= Start, I \= End -> OutFlow[I]-InFlow[I] #= 0 ; true)),
        sum([X[I,Start] : I in 1..N, C[I,Start]>0]) #= 0 ,
        sum([X[End,J] : J in 1..N, C[End,J]>0]) #= 0,

        term_variables([XVars,InFlow,OutFlow], Vars),
        maxof(labeling(Vars),Total),

        writeln(total:Total),
        Rows @= X^rows,
        foreach(Row in Rows,
                (foreach(R in Row, format("~3d ",[R])), nl)),
        writeln(inFlow:InFlow),
        writeln(outFlow:OutFlow),

        nl.
