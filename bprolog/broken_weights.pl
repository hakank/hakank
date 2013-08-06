/*

  Broken weights problem in B-Prolog.

  From
  http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first 
  posed by Claude Gaspard Bachet de Méziriac in a book of arithmetic problems 
  published in 1612, and can also be found in Heinrich Dorrie’s 100 
  Great Problems of Elementary Mathematics.
  
      A merchant had a forty pound measuring weight that broke 
      into four pieces as the result of a fall. When the pieces were 
      subsequently weighed, it was found that the weight of each piece 
      was a whole number of pounds and that the four pieces could be 
      used to weigh every integral weight between 1 and 40 pounds. What 
      were the weights of the pieces?
  
  Note that since this was a 17th-century merchant, he of course used a 
  balance scale to weigh things. So, for example, he could use a 1-pound 
  weight and a 4-pound weight to weigh a 3-pound object, by placing the 
  3-pound object and 1-pound weight on one side of the scale, and 
  the 4-pound weight on the other side.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        N = 4,
        M = 40,

        broken_weights(N,M,Weights,X),
        writeln(Weights),
        print_matrix(X).


% alternative version of the problem:
% what is the minimal number of weights
% for weighting all values of 1..M
% Also, we minimize the last value of the
% weights.
go2 :-
        N :: 1..40,
        indomain(N),
        writeln(n:N),

        M = 80,

        broken_weights(N,M,Weights,X,true),
        writeln(Weights),
        print_matrix(X).

        
% default call
broken_weights(N,M, Weights, X) :-
        broken_weights(N,M, Weights, X, false).


% Minimize = true -> minimize the last value of Weights
broken_weights(N,M, Weights, X, Minimize) :-

        length(Weights, N),
        Weights :: 1..M,

        new_array(X,[M,N]),
        array_to_list(X,XVar),
        XVar :: -1..1,
        % symmetry breaking
        increasing(Weights),

        M #= sum(Weights),
        foreach(J in 1..M,[Slice],
                (
                    Slice @= [X[J,I] : I in 1..N],
                    product_lists(Slice,Weights,J)
                )
        ),
        
        % search
        term_variables([Weights,XVar], Vars),

        (Minimize = true -> 
             % minimize the last weights
             LastWeight @= Weights[N],
             minof(labeling(Vars), LastWeight)
        ;
             labeling(Vars)
        ).


print_matrix(X) :-
        Rows @= X^rows,
        length(Rows,Len),
        foreach((I,Row) in (1..Len,Rows), 
                (
                    format("~2d: ", [I]),
                    foreach(R in Row, format("~3d", [R])),
                    nl
                )).

product_lists(List1, List2, Sum) :-
        Sum #= sum([L1*L2 : (L1,L2) in (List1, List2)]).


increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).

