/*

  Costas array in B-Prolog.

  From http://mathworld.wolfram.com/CostasArray.html:
  """
  An order-n Costas array is a permutation on {1,...,n} such
  that the distances in each row of the triangular difference
  table are distinct. For example, the permutation {1,3,4,2,5}
  has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
  and {4}. Since each row contains no duplications, the permutation
  is therefore a Costas array.
  """
  Also see
  http://en.wikipedia.org/wiki/Costas_array

  This model is based on Barry O'Sullivan's MiniZinc model
  (http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn)
  Here are the two rather simple differences 
  (marked by "hakank" below)
   1) no symmetry breaking on the order of the Costas array
   2) fixes the lower triangular matrix in the difference
      matrix to -n+1
  
  Since there is no symmetry breaking of the order of the Costas 
  array it gives all the solutions for a specific length of 
  the array, e.g. those 
  listed in http://mathworld.wolfram.com/CostasArray.html
  
  1	1	(1)
  2	2	(1, 2), (2,1)
  3	4	(1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
  4	12	(1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4), 
                (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1), 
                (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
  ....
  
  See http://www.research.att.com/~njas/sequences/A008404
  for the number of solutions for n=1..
  1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828, 
  17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052, 
  872, 200, 88, 56, 204,...
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 8,
        findall(Costas,costas(N,Costas,print),L),
        length(L,Len),
        writeln(len:Len),
        nl.
     
go2 :-
        foreach(N in 2..10,
                [Costas,L,Len],
                ac(Seq,[]),
                (
                    writeln(n:N),
                    findall(Costas,costas(N,Costas,noprint),L) -> 
                        length(L,Len),
                        writeln(len:Len),
                        Seq^1 = [Len|Seq^0],
                        nl
                ;
                        true
                )),
        reverse(Seq,Seq2),
        writeln(sequence:Seq2),
        nl.
                    


costas(N,Costas,Print) :-
   
        length(Costas,N),
        Costas :: 1..N,

        new_array(Differences,[N,N]),
        term_variables(Differences,DiffVars),
        DiffVars :: -N+1..N-1,
        
        %
        % hakank: Here are my two changes
        %
        % 1) I skipped this constraint since I want 
        %    to generate all solutions.
        % Costas[1] #< Costas[N],
        
        % 2) Fix the values in the lower triangle in the
        % difference matrix to -n+1. This removes variants 
        % of the difference matrix for the the same Costas array.
        foreach(I in 1..N, J in 1..I, Differences[I,J] #= -N+1),

        % hakank: All the following constraints are from 
        % Barry O'Sullivans's original MiniZinc model.
        alldifferent(Costas),

        % "How do the positions in the Costas array relate 
        %  to the elements of the distance triangle."
        foreach(I in 1..N, J in I+1..N,
                Differences[I,J] #= Costas[J] - Costas[J-I]),

        % "All entries in a particular row of the difference 
        %  triangle must be distint."
        foreach(I in 1..N-1,[Temp],
                (Temp @= [Differences[I,J] : J in I+1..N],
                 alldifferent(Temp))
               ),

        % "All the following are redundant - only here to speed up search."
        
        % "We can never place a 'token' in the same row as any other."
        foreach(I in 1..N, J in I+1..N,  Differences[I,J] #\= 0),

        foreach(K in 3..N, L in K+1..N,
                (Differences[K-2,L-1] + Differences[K,L] #= 
                 Differences[K-1,L-1] + Differences[K-1,L])
               ),
        
        term_variables([Costas,DiffVars],Vars),
        labeling(Vars),
        (Print = print ->
             writeln(costas:Costas),
             foreach(I in 1..N, 
                     (foreach(J in 1..N, [D],
                              (D @= Differences[I,J],
                               D > -N+1 ->
                                   format("~2d ", [D])
                              ;
                                   write(' ')
                              )),
                      nl)
                    ),
             nl
        ;
             true
        ).

        