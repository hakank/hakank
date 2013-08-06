/*

  Broken weights problem in ECLiPSe.

  
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

  Also, compare with the MiniZinc model:
    http://www.hakank.org/minizinc/broken_weights.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).


go :-
        N = 4,
        M = 40,

        broken_weights(N,M,Weights,X,Backtracks),
        writeln(Weights),
        writeln(backtracks:Backtracks),
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

        broken_weights(N,M,Weights,X,Backtracks,true),
        writeln(Weights),
        writeln(backtracks:Backtracks),
        print_matrix(X).

        
% default call
broken_weights(N,M, Weights, X, Backtracks) :-
        broken_weights(N,M, Weights, X, Backtracks,false).


% Minimize = true -> minimize the last value of Weights
broken_weights(N,M, Weights, X, Backtracks,Minimize) :-
        
        dim(Weights, [N]),
        Weights[1..N] :: 1..M,
        collection_to_list(Weights,WeightsList),

        dim(X,[M,N]),
        X[1..M,1..N] :: -1..1,

        % symmetry breaking
        ordered(=<, Weights),

        M #= sum(WeightsList),

        ( for(J,1,M),
          param(X,WeightsList,N) do
              Slice is X[J,1..N],
              product_lists(Slice,WeightsList,J)
        ),
        
        % search
        term_variables([Weights,X], Vars),

        % Just find any solution
        (Minimize = true -> 

             % minimize the last weights

             LastWeight is Weights[N],
             minimize(search(Vars, 0, occurrence, indomain_min, complete,
                             [backtrack(Backtracks)]), LastWeight)
             % bb_min(search(Vars, 0, occurrence, indomain_min, complete,
             %       [backtrack(Backtracks)]), LastWeight, bb_options{strategy:restart}).
        ;
             search(Vars, 0, occurrence, indomain_min, complete,
               [backtrack(Backtracks)])
        ).




print_matrix(X) :-
        dim(X,[M,_N]),
        ( for(J,1,M),
          foreacharg(Row, X) do
              collection_to_list(Row,List),
              printf("%3d: ", J),
              ( foreach(L,List) do
                    printf("%3d", L)
              ),
              nl
        ).


product_lists(List1, List2, Sum) :-
        ( foreach(L1,List1),
          foreach(L2,List2),
          fromto(0,In,Out,Sum) do
              Out #= In + L1*L2
        ).