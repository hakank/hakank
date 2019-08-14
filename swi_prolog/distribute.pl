/*

  Global constraint distribute in SWI Prolog

  Decomposition of global constraint distribute.

  From MiniZinc globals.mzn (slighly edited:
  """
  Requires that Card[I] is the number of occurences of Value[I] in list X.
  """

  Note: distribute/3 is a variant of the global constraint
  global cardinality count (global_cardinality, gcc).

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


/*
   Unique solution:
  
   card=[4,1,1,1]
   value=[6,7,8,9]
   x=[6,7,6,8,6,9,6]
 
*/
go :-
        
        %% This test is from MiniZinc's testcases.
        Len = 4,
        length(Card,Len), 
        Card ins 1..10, 
        length(Value,Len),
        Value ins 1..10,
        length(X,7),
        X ins 1..10,
        
        %% test values
        Card  = [4, _, 1, _],
        Value = [_, 7, 8, _],
        X  = [_, 7, 6, 8, 6, 9, _],

        distribute(Card, Value, X),
   
        flatten([Card,Value,X],Vars),
        labeling([ff],Vars),

        writeln(value=Value),
        writeln(card=Card),
        writeln(x=X),
        nl.

%%
%% No initial values of Card and Value. A lot of solutions.
%%
go2 :-

        %% This test is from MiniZinc's testcases.
        Len = 5,
        length(Card,Len), 
        Card ins 1..10, 
        length(Value,Len),
        Value ins 1..10,
        
        length(X,7),
        X ins 1..10,
        
        distribute(Card, Value, X),
   
        flatten([Card,Value,X],Vars),
        labeling([ff],Vars),

        writeln(value=Value),
        writeln(card=Card),
        writeln(x=X),
        nl,
        fail,
        nl.

go2.


%%
%% A larger example.
%%
go3 :-

        Value = [0,1,2,3,5,7],
        Card  = [1,3,1,1,1,1],
        
        length(X,18),
        X ins 0..9,
        
        distribute(Card, Value, X),
   
        flatten([Card,Value,X],Vars),
        labeling([],Vars),

        writeln('value'=Value),
        writeln('card '=Card),
        writeln('    x'=X),
        nl,
        fail,
        nl.

go3.

