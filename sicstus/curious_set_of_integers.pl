/*

  Curious set of integers problem in SICStus Prolog.

  Martin Gardner (February 1967):
  """
  The integers 1,4,9, and 120 form a set with a remarkable property:
  the product of any two integers is one less than a perfect square. 
  Find a fifth number that can be added to the set without destroying 
  this property.
  """
  
  Solution: The number is 0.
 
  There are however other sets of five numbers with this property.
  Here are the one in the range of 0.10000:
  [0, 1, 3, 8, 120]
  [0, 1, 3, 120, 1680]
  [0, 1, 8, 15, 528]
  [0, 1, 8, 120, 4095]
  [0, 1, 15, 24, 1520]
  [0, 1, 24, 35, 3480]
  [0, 1, 35, 48, 6888]
  [0, 2, 4, 12, 420]
  [0, 2, 12, 24, 2380]
  [0, 2, 24, 40, 7812]
  [0, 3, 5, 16, 1008]
  [0, 3, 8, 21, 2080]
  [0, 3, 16, 33, 6440]
  [0, 4, 6, 20, 1980]
  [0, 4, 12, 30, 5852]
  [0, 5, 7, 24, 3432]
  [0, 6, 8, 28, 5460]
  [0, 7, 9, 32, 8160]

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/curious_set_of_integers.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/curious_set_of_integers.ecl 


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 5, 
        findall(X,curious_set_of_integers(N, X),List),
        ( foreach(L,List) do
              write(L), nl
        ),
        nl, fd_statistics.


curious_set_of_integers(N, X) :-
        length(X, N),
        domain(X,0,10000),

        all_distinct(X),
        my_ordered(X),
        ( foreach(X1,X),
          count(I,1,_),
          param(X) do
              ( foreach(X2,X),
                count(J,1,_),
                param(X1,I) do
                    I \= J 
              ->
                P in 0..1000,
                P*P-1 #= X1*X2
              ;
                true
              )
        ),

        labeling([ffc,bisect,up],X),
        write(X),nl,
        fd_statistics.

        


my_ordered(List) :-
        ( List = [] 
        -> 
          true 
        ;
          ( fromto(List, [This,Next | Rest], [Next|Rest],[_])
          do
            This #< Next
          )
        ).
