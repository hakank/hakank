/*

  Just forgotten puzzle (Enigma 1517) in SICStus Prolog.

  From http://www.f1compiler.com/samples/Enigma 201517.f1.html
  """
 
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
 
  
  Joe was furious when he forgot one of his bank account numbers. 
  He remembered that it had all the digits 0 to 9 in some order, so he tried
  the following four sets without success:
 
      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7 
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5
 
  When Joe finally remembered his account number, he realised that in each set
  just four of the digits were in their correct position and that, if one knew
  that, it was possible to work out his account number.
  What was it?
 
  """
 
  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/just_forgotten.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        A = [[9,4,6,2,1,5,7,8,3,0],
             [8,6,0,4,3,9,1,2,5,7],
             [1,6,4,0,2,9,7,8,5,3],
             [6,8,2,4,3,1,9,0,7,5]],

        length(Xs,10),
        domain(Xs,0,9),

        all_different(Xs),
       
        ( foreach(Row,A),
          param(Xs) do
              ( foreach(R,Row),
                foreach(X,Xs),
                fromto(Sum,Out,In,[]) do
                    Reif in 0..1,
                    X #= R #<=> Reif #= 1,
                    Out = [Reif|In]
                ),
              sum(Sum,#=,4)
        ),

        labeling([ff,bisect,down], Xs),
        
        write(Xs),nl,
        fd_statistics.


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).

