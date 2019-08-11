/*

  Broken weights problem in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

% :- table broken_weights_rec/2. % Not needed.

go :-
   N = 4,
   M = 40,
   broken_weights(N,M,Weights,X),
   writeln(weights=Weights),
   writeln("the matrix:"),
   print_solution(X),
   nl.

%%
%% Alternative version of the problem:
%% what is the minimal number of weights
%% for weighting all values of 1..80.
%% We don't minimize the last value of the
%% weights, so any value is good.
%%
%% One answer for N=80;
%% It needs 5 weights, for example [1,2,6,18,53]
%%
go2 :-
   M = 80,
   N in 1..M,
   indomain(N),
   writeln(n=N),

   broken_weights(N,M,Weights,X,false),
   writeln(Weights),
   print_solution(X),
   nl.

%%
%% What is the minimal number of weights
%% for weighting all values of 1..80?
%% Also, we minimize the last value of the
%% weights.'
%%
%% (The difference between this problem and go2/0 is
%% that we here minimize the last value,)
%%
%% Weights [1,3,9,27,40]
%%
%% (This takes about 1:30minutes.)
%%
go3 :-
        M = 80,
        N in 1..40,
        indomain(N),
        writeln(n=N),

        broken_weights(N,M,Weights,X,true),
        writeln(Weights),
        print_solution(X).

%%
%% Recursive variant.
%% From http://stackoverflow.com/questions/38468488/how-to-program-this-puzzle-recursively
%%
%% """
%% So, the answer is (1,3,9,27) which can be generalized as twice the sum of previous terms + 1.
%% """
%% Note that this don't solve the minimization problem, just the satisfiability problem.
%%
%% For N in 1..40:
%% [1,3,9,27,81,243,729,2187,6561,19683,59049,177147,531441,1594323,4782969,14348907,43046721,129140163,387420489,1162261467,3486784401,10460353203,31381059609,94143178827,282429536481,847288609443,2541865828329,7625597484987,22876792454961,68630377364883,205891132094649,617673396283947,1853020188851841,5559060566555523,16677181699666569,50031545098999707,150094635296999121,450283905890997363,1350851717672992089,4052555153018976267]
%%
%% (It don't need to be tabled.)
%%
go4 :-
        findall(Res,(between(1,40,N),broken_weights_rec(N,Res)),L),
        writeln(L),
        nl.

%%   
%%% default call
%%
broken_weights(N,M, Weights, X) :-
   broken_weights(N,M, Weights, X, true).

%%
%% Minimize = true -> minimize the last value of Weights
%%
broken_weights(N,M, Weights, X, Minimize) :-

        length(Weights,N),
        Weights ins 1..M,
        
        new_matrix(M,N,-1..1,X),
        
        all_distinct(Weights),
        
        %% symmetry breaking
        increasing_strict(Weights),
        
        sum(Weights,#=,M),
   
        findall(J,
                between(1,M,J),
                Js),
        maplist(scalar_product_list(Weights),X,Js),
   
        %% Minimize last weight
        element(N,Weights,WeightsN),
        
        flatten([Weights,X],Vars),
        (
         Minimize == true
        ->
         %% minimize the last weights
         labeling([min(WeightsN),enum], Vars)
        ;
         labeling([enum],Vars)
        ).

%%
%% Sum Weights and each slice of X to give this value J
%%
scalar_product_list(Weights,X,J) :-
        scalar_product2(Weights,X,J).


%%
%% Recursive version of the optimal (last) value of the broken weight problem
%% for N different weight
%% It don't need to be tables.
%%
broken_weights_rec(N, Res) :-
        broken_weights_rec1(N,_,Res).
broken_weights_rec1(N,1,1) :- N #=< 1,!.
broken_weights_rec1(N,NewSum,NewTerm) :-
        N1 #= N-1,
        broken_weights_rec1(N1,OldSum, _OldTerm),
        NewTerm #= 2*OldSum + 1,
        NewSum #= OldSum+NewTerm.

print_solution(X) :-
        length(X,Len),
        numlist(1,Len,Nums),
        transpose([Nums,X],Vars),
        % maplist(format("~d: ~w~n"),Vars).
        maplist(print_row,Vars).

print_row(Row) :-
        flatten(Row,Row2),
        format("~t~d~4|: ~t~d~10| ~t~d~15| ~t~d~20| ~t~d~25| ~n", Row2).