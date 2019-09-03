/*

  Perfect square sequence in SWI Prolog

  From "Fun with num3ers"
  "Sequence"
  http://benvitale-funwithnum3ers.blogspot.com/2010/11/sequence.html
  """
  If we take the numbers from 1 to 15 
      (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
  and rearrange them in such an order that any two consecutive 
  numbers in the sequence add up to a perfect square, we get,
  
  8     1     15     10     6     3     13     12      4      5     11     14        2      7      9
      9    16    25     16     9     16     25     16     9     16     25     16       9     16
1  
  
  I ask the readers the following:
  
  Can you take the numbers from 1 to 25 to produce such an arrangement?
  How about the numbers from 1 to 100?
  """
  
  Via http://wildaboutmath.com/2010/11/26/wild-about-math-bloggers-111910

  The first solution is for N=15:
    x=[8,1,15,10,6,3,13,12,4,5,11,14,2,7,9]
    tmp=[9,16,25,16,9,16,25,16,9,16,25,16,9,16]


  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   between(2,50,N),
   writeln(n=N),
   time(once(perfect_square_sequence(N))),
   fail,
   nl.

go.

perfect_square_sequence(N) :-

        squares(N,Squares),

        %% decision variables
        length(X,N),
        X ins 1..N,
   
        N1 #= N-1,
        length(Tmp,N1),
        %% Convert [1,4,9,...] to a domain 1\/4\/9...
        list_domain_disjunction(Squares,SquaresDomain),
        Tmp ins SquaresDomain,
        
        %% constraints
        all_distinct(X),
        
        numlist(2,N,Is),
        maplist(seq(X,Tmp),Is),
   
        %% symmetry breaking
        append([X1|_],[XN],X),
        X1 #< XN,

        flatten([X,Tmp],Vars),
        labeling([], Vars),

        writeln(x=X),
        writeln(tmp=Tmp),
        nl.

   % foreach(I in 2..N) 
   %   Tmp[I-1] #= (X[I-1]+X[I])
   % end,
seq(X,Tmp,I) :-
        I1 #= I-1,
        element(I1,Tmp,TmpI1),
        element(I1,X,XI1),
        element(I,X,X1),
        TmpI1 #= XI1+X1.

pow2(A,C) :- C #= A*A.
squares(Limit,Squares) :-
        numlist(1,Limit,Is),
        maplist(pow2,Is,Squares).
