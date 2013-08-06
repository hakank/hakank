/*

  Knights path problem in ECLiPSe.
  
  Create a knights path in a n x n matrix for all integers from 1..n*n-1.
  The integer n*n is placed whatever it may fit...

  
  Compare with the the following models:
  * MiniZinc: http://www.hakank.org/minizinc/knight_path.mzn
  * Comet   : http://www.hakank.org/comet/knights_path.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(listut).
:-lib(propia).

go :-
        N = 5,
        knights_path(N, X, Backtracks),
        pretty_print(X), 
        writeln(backtracks:Backtracks),
        nl, fail.

knights_path(N, X, Backtracks) :-
        
        N2 is N*N,
        dim(X,[N,N]),
        
        X :: 1..N2,

        C = [-2,-1,1,2],

        flatten_array(X,XList),

        ic_global:alldifferent(XList),


        X[1,1] #= 1,
     
        %
        %  place all integers 1..n^2
        %
        % Note: This is (almost) exactly the same as the loop in
        % http://www.hakank.org/eclipse/hidato.ecl 
        % except for the abs constraint.
        % 
        % NN2 is N*N-1,
        NN2 is N*N-1,
        ( for(K,1,NN2),
          param(XList,C,N) do

          % define temporal variables for finding
          % the index of this and the next number (K)
          I :: 1..N, % index I
          J :: 1..N, % index J
          A :: C, % offset from K's position
          B :: C, % ibid
          
          % needed for nth1
          IA #= I+A,
          JB #= J+B,
          
          % some extra constraints
          IA #>= 1,
          JB #>= 1,
          IA #=< N,
          JB #=< N,
          abs(A)+abs(B) #= 3, % it must be a knight move, i.e. 2+1 = 3
          % moves(A,B) infers most,

          % 1) First: fix this k, i.e.
          % K #= X[I,J], % don't work (instantiation fault)
          IJ #= (I-1)*N + J,
          nth1(IJ, XList, K),


          % 2) Then, find the position of the next value, i.e.
          % K+1 #= X[I+A,J+B], % don't work
          IA_JB #= (I-1+A)*N + J+B,
          K1 is K+1,
          nth1(IA_JB, XList, K1)

        ),

        

        search(XList,0,occurrence,indomain_min,complete,[backtrack(Backtracks)]).

        

pretty_print(X) :-
        dim(X, [N,N]),
        ( for(I, 1, N), param(X, N) do
            ( for(J, 1, N), param(X, I) do
                XX is X[I,J],
                printf("%2d", XX),
                write(" ")
            ),
            nl
        ).        

moves(-1,-2).
moves(-1,2).
moves(1,-2).
moves(1,2).
moves(-2,-1).
moves(-2,1).
moves(2,-1).
moves(2,1).
