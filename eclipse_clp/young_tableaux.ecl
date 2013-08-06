/*

  Young tableaux in ECLiPSe.

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
 
  And the corresponding standard Young tableaux are:
 
  1.   1 2 3 4
 
  2.   1 2 3         1 2 4    1 3 4
       4             3        2
 
  3.   1 2           1 3
       3 4           2 4
 
  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3
 
  5.   1
       2
       3
       4
  """  
  

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/young_tableaux.mzn
  * Choco   : http://www.hakank.org/choco/YoungTableuax.java
  * JaCoP   : http://www.hakank.org/JaCoP/YoungTableuax.java
  * Comet   : http://www.hakank.org/comet/young_tableaux.co
  * Gecode  : http://www.hakank.org/gecode/young_tableaux.cpp



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
% :-lib(branch_and_bound).



go :-
        N = 6,
        young_tableaux(N), nl, fail.



young_tableaux(N) :-

        printf("Young tableaux and partitions of order %d\n", [N]),
        dim(X, [N,N]),
        X :: 1..N+1,

        % the partition structure
        dim(P,[N]),
        P :: 0..N+1,

        % 1..N is used exactly once (N+1 may be used many times
        ( for(I,1,N), param(X) do
              ic_global:occurrences(I, X, 1)
        ),

        X[1,1] #= 1,

        % all rows and columns should be ordered
        ( for(I,1,N), param(X,N) do
              ordered(=<, X[I,1..N]), % rows
              ordered(=<, X[1..N,I])  % columns
        ),

        % calculate the structure (the partition)
        % forall(i in 1..n) (
        % p[i] = sum(j in 1..n) (bool2int(x[i,j] <= n))
        % )
        ( for(I,1,N), 
          param(X,N,P) do
              ( for(J,1,N), param(X,N,I),
                    fromto(0,In,Out,Sum) do
                    Out #= In + (X[I,J] #=< N)
              ),
              P[I] #= Sum
        ),

        flatten_array(P,PList),
        sum(PList) #= N,

        % P should be ordered
        ordered(>=,P),

        term_variables([X,P], Vars),

        search(Vars,0,first_fail,indomain,complete,[backtrack(Backtracks)]),

        writeln(p:PList),
        pretty_print(X),

        writeln(backtracks:Backtracks).



pretty_print(X) :-
        dim(X, [N,N]),
        ( for(I, 1, N), param(X, N) do
              ( for(J, 1, N), param(X, I,N) do
                    XIJ is X[I,J],
                    (XIJ #=< N -> printf("%2d", [XIJ]) ; true),
                    write(" ")
              ),
              % don't show "empty" lines
              X[I,1] #=< N -> nl ; true
        ),nl.


