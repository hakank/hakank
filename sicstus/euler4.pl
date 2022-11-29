/*

  Euler Problem 4 in SICStus Prolog.

  http://projecteuler.net/index.php?section=problems&id=4
  """
  A palindromic number reads the same both ways. The largest palindrome
  made from the product of two 2-digit numbers is 9009 = 91*99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sictsu_prolog/

*/

:- ensure_loaded(hakank_utils).

go :-
        L = [
             % euler4a,
             % euler4b,
             % euler4c,
             euler4d % ,
             % euler4e
            ],
        run_problems(L).

% 0.220s
euler4a :-
        From = 100,
        To = 999,        
        findall(IJ,
                (between(From,To,I),
                 between(I,To,J),
                 IJ is I*J,
                 palindromic2(IJ)
                ),
                IJs),
        max_member(Max,IJs),
        writeln(Max).

%%
%% Skipping findall/3 and between/3, and rolling
%% a double loop manually.
%% Slightly slower: 0.018s
%%
euler4b :-
        From = 100,
        To = 999,
        e4b(From,From,From,To,0,Max),
        writeln(Max).

e4b(To,_FromBase,To,To,Max,Max).
e4b(From,FromBase,To0,To,Max0,Max) :-
        From =< To0,
        T is To0*From,
        (
         (T > Max0, palindromic2(T))
        ->
         Max1 = T
        ;
         Max1 = Max0
        ),
        From1 is From + 1,
        e4b(From1,FromBase,To0,To,Max1,Max).
% Reset From
e4b(From,FromBase,To0,To,Max0,Max) :-
        From > To0,
        To1 is To0 + 1,
        e4b(FromBase,FromBase,To1,To,Max0,Max).

% 0.001s
euler4c :-
    euler4c(P),    
    writeln(P).


% CP approach
% From
% https://stackoverflow.com/questions/68324157/solving-euler-4-with-clp
% 
euler4c(P) :-
  A in 1..9,
  B in 0..9,
  C in 0..9,
  D in 100..999,
  E in 100..999,
  E #>= D,
  P #= D * E,
  P #= A * 100001 + B * 10010 + C * 1100,  
  labeling([maximize(P),ff,bisect,down], [A,B,C,D,E,P]). 


% 0.001s
euler4d :-
    euler4d(P),    
    writeln(P).


%%
%% Using domain/3 instead of in/2 (no difference in solving time).
%% 
euler4d(P) :-
  domain([A],1,9),
  domain([B,C],0,9),
  domain([D,E], 100,999),
  E #>= D,
  P #= D * E,
  P #= A * 100001 + B * 10010 + C * 1100,  
  labeling([maximize(P),ff,bisect,down], [A,B,C,D,E,P]). 

%%
%% for/3 loops
%% 0.034s
%%
euler4e :-
    From = 100,
    To = 999,
    bb_put(max,0),
    (for(I,From,To), param(To) do
     (for(J,I,To), param(I) do
      IJ is I*J,
      bb_get(max,MaxVal),
      ((IJ > MaxVal, palindromic2(IJ)) ->
          bb_put(max,IJ)
      ;
          true
      )
     )
    ),
    bb_get(max,Max),
    writeln(Max).

