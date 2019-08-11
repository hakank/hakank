/*

  Sicherman Dice problem in SWI Prolog

  From http://en.wikipedia.org/wiki/Sicherman_dice
  """ 
  Sicherman dice are the only pair of 6-sided dice which are not normal dice, 
  bear only positive integers, and have the same probability distribution for 
  the sum as normal dice.
  
  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  """

  I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/

  This model gets the two different ways, first the standard way and
  then the Sicherman dice:
  
  x1 = array1d(1..6, [1, 2, 3, 4, 5, 6]);
  x2 = array1d(1..6, [1, 2, 3, 4, 5, 6]);
  ----------
  x1 = array1d(1..6, [1, 2, 2, 3, 3, 4]);
  x2 = array1d(1..6, [1, 3, 4, 5, 6, 8]);


  Extra: If we also allow 0 (zero) as a valid value then the 
  following two solutions are also valid:
  
  x1 = array1d(1..6, [0, 1, 1, 2, 2, 3]);
  x2 = array1d(1..6, [2, 4, 5, 6, 7, 9]);
  ----------
  x1 = array1d(1..6, [0, 1, 2, 3, 4, 5]);
  x2 = array1d(1..6, [2, 3, 4, 5, 6, 7]);
  
  These two extra cases are mentioned here:
  http://mathworld.wolfram.com/SichermanDice.html



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        % sicherman_dice(X1,X2,1),
        % writeln(x1=X1),
        % writeln(x2=X2),
        findall(_, sicherman_dice(_X1a,_X2a,1),_L1),
        nl,
        findall(_, sicherman_dice(_X1b,_X2b,0),_L2),
        nl.
      

%%
%% Sicherman dice.
%%
%% Ensure that the two dice (X1, X2) are distributed according to
%% standard two dice distribution.
%%
%% Min: The minimum number of spots on each die (0 or 1).
%%
sicherman_dice(X1,X2, Min) :-
        
        format("\nMin: ~d\n", Min),
        
        N = 6,                  % number of dice
        M = 10,                 % max value of each side

        %% standard distribution of 2 pair of dice
        StandardDist = [1,2,3,4,5,6,5,4,3,2,1],


        length(X1,N),
        length(X2,N),
        X1 ins Min..M,
        X2 ins Min..M,

        %% ensure standard distributions of the sums
        numlist(1,10,Ks),
        maplist(standard_dist(X1,X2,StandardDist),Ks),
        
        %% Symmetry breaking
        increasing(X1),
        increasing(X2),

        %% x1 is less <= to x2
        maplist(lex_lte,X1,X2),

        flatten([X1,X2],Vars),
        labeling([min], Vars),

        writeln(X1),
        writeln(X2),
        nl.

%%
%% Ensure that all the numbers (Ks) are according to
%% the standard distribution.
%%
standard_dist(X1,X2,StandardDist,K) :-
        length(X1,N),
        numlist_cross2(N,N,IJs),
        element(K,StandardDist,SDK),
        dist_k(IJs,K,X1,X2,0,SDK).

%%
%% The distribution for this K (or rather K+1)
%%
dist_k([],_K,_X1,_X2,Sum,Sum).
dist_k([[I,J]|IJs],K,X1,X2,Sum0,Sum) :-
        element(I,X1,X1I),
        element(J,X2,X2J),
        B in 0..1, 
        (X1I + X2J #= K+1) #<==> B #= 1,
        Sum1 #= Sum0 + B,
        dist_k(IJs,K,X1,X2,Sum1,Sum).

