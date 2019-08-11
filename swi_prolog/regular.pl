/*

  Decomposition of global constraint regular in SWI Prolog

  Here's a description of the transition tables.
  Note that input can never be 0 since it is used for no state.

  Example: regexp 123*21

     Transition = [
       % inputs: 1  2  3 
                [2, 0, 0], % transitions from state 1: -2-> state 2  [start]
                [0, 3, 0], % transitions from state 2: -3-> state 3 % 
                [0, 4, 3], % transitions from state 3: -3-> state 3, -2-> state 4
                [5, 0, 0], % transitions from state 4: -1-> state 5  [final]
                [0, 0, 0]  % transitions from state 5: (none)
                ],
     InitialState = 1,
     AcceptingStates = [5]


  The states are numbered from 1..Transition.length.
  The initial state is in InitialState, i.e. the state Transition[InitialState].
  The columns are the inputs (the column index), e.g. the first state ([2, 0, 0]) 
  means that it only accept a "1" (first column), and then go to state 2 ([0, 3, 0],
  which only accept a "2" (second column) and go to state 3 ([0, 4, 3]) with the
  following meaning:

      - if "1": not accepted
      - if "2": goto state 4
      - if "3": goto state 3 (loop)

  And the DFA now goto through the states. The accepting state in this
  example is only state 5 (AcceptingStates must be a list).

  
  Note: This implementation of regular/6 is a port of my Picat
  implementation (http://hakank.org/picat/regular.pi)
  which is a port of the MiniZinc standard implementation
  - via my or-tools/python version hakank.org/google_or_tools/regular.py
  - via my Comet implementation http://hakank.org/comet/regular.co
  I might have forgot some transition step here :-).


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Testing regexp 123*21
%%
go :-

   dfa(1,Transition,NStates,InputMax,InitialState,AcceptingStates),

   % decision variables
   between(4,10,N),
   writeln(n=N),

   length(X,N),
   X ins 1..3,
   regular(X,NStates,InputMax,Transition,InitialState, AcceptingStates),

   labeling([ff,bisect], X),

   writeln('x  '=X),
   fail,
   nl.

go.




%
% same as go/0 but we also get the state list (A)
% using regular2/7.
%
go1b :-

   dfa(1,Transition,NStates,InputMax,InitialState,AcceptingStates),

   % decision variables
   N in 2..10,
   indomain(N),
   writeln(n=N),

   length(X,N),
   X ins 1..3,

   regular2(X,NStates,InputMax,Transition,InitialState, AcceptingStates,A),

   labeling([ff], X),

   writeln('x  '=X),
   writeln('A  '=A),
   fail,
   nl.

go1b.

%%
%% aa*bb* + cc*
%%
go2 :- 

   dfa(2,Transition,NStates,InputMax,InitialState,AcceptingStates),

   N = 11,

   length(X,N),
   X ins 1..3,

   regular(X,NStates,InputMax,Transition,InitialState, AcceptingStates),

   labeling([ff], X),

   writeln('x  '=X),
   fail,
   nl.


go2.

%%
%% From Choco (see below), dfa(3,...)
%%
go3 :- 

   dfa(3,Transition,NStates,InputMax,InitialState,AcceptingStates),
   N = 10,
   length(X,N),
   X ins 1..3,
   regular(X,NStates,InputMax,Transition,InitialState, AcceptingStates),
   labeling([ff], X),
   writeln('x  '=X),
   fail,
   nl.

go3.

%%
%% All equal constraint
%%
go4 :- 

   dfa(4,Transition,NStates,InputMax,InitialState,AcceptingStates),
   N = 10,
   length(X,N),
   X ins 1..3,

   regular(X,NStates,InputMax,Transition,InitialState, AcceptingStates),

   labeling([ff], X),

   writeln('x  '=X),
   fail,
   nl.

go4.

%%
%% This should be interpreted as:
%%  0{2}1{2,3}2{2}
%% for all permutations of 0,1,and 2.
%% Or rather:
%%  1{2}2{2,3}3{2}
%%
go5 :- 

   dfa(5,Transition,NStates,InputMax,InitialState,AcceptingStates),

   N = 8,
   length(X,N),
   X ins 1..3,

   regular(X,NStates,InputMax,Transition,InitialState, AcceptingStates),

   labeling([ff], X),

   writeln('x  '= X),
   fail,
   nl.

go5.

%%
%% All different.
%% This works in principle (here is n=3), but the automaton will 
%% be forbiddingly large for larger arrays.
%% 
go6 :- 

   dfa(6,Transition,NStates,InputMax,InitialState,AcceptingStates),

   N = 3,
   length(X,N),
   X ins 1..3,

   regular(X,NStates,InputMax,Transition,InitialState, AcceptingStates),

   Vars = X,
   labeling([ff], Vars),

   writeln('x  '=X),
   fail,
   nl.

go6.


%
% same as go6/0 but also exposing A, the state array.
%
go6b :- 

   dfa(6,Transition,NStates,InputMax,InitialState,AcceptingStates),

   N = 3,
   length(X,N),
   X ins 1..3,

   regular2(X,NStates,InputMax,Transition,InitialState, AcceptingStates,A),

   labeling([ff], X),

   writeln('x  '=X),
   writeln('A  '=A),
   nl,
   fail,
   nl.

go6b.



%
% even number of 1s and 2s
%
go7 :- 

   dfa(7,Transition,NStates,InputMax,InitialState,AcceptingStates),

   N = 10,
   length(X,N),
   X ins 1..2,

   regular(X,NStates,InputMax,Transition,InitialState, AcceptingStates),

   labeling([ff], X),

   writeln('x  '=X),
   %%writeln(['1s'=sum([1 : E in X, E =1]),'2s'=sum([1 : E in X, E = 2])]),
   count_occurrences(X,1,Count1s),
   writeln('1s'=Count1s),
   count_occurrences(X,2,Count2s),
   writeln('2s'=Count2s),
   nl,
   fail,
   nl.

go7.


%%
%% Note: The implementation of regular/6 and regular2/7
%% has been placed in hakank_utils.pl
%%

% /*

%   regular(X, Q, S, D, Q0, F)
  
%   This is a translation of MiniZinc's regular constraint (defined in
%   lib/zinc/globals.mzn), via the Comet code refered above.
%   All comments in '"""' are from the MiniZinc code.
  
%   Note: This is a translation of my Picat implementation
%         (http://hakank.org/picat/regular.pi)
%   """
%   The sequence of values in array 'x' (which must all be in the range 1..S)
%   is accepted by the DFA of 'Q' states with input 1..S and transition
%   function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
%   (which must be in 1..Q) and accepting states 'F' (which all must be in
%   1..Q).  We reserve state 0 to be an always failing state.
%   """
  
%   x : IntVar array
%   Q : number of states
%   S : input_max
%   d : transition matrix
%   q0: initial state
%   F : accepting states
  
% */
% regular(X, Q, S, D, Q0, F) :-

%         %% """
%         %% If x has index set m..n-1, then a[m] holds the initial state
%         %% (q0), and a[i+1] holds the state we're in after  processing
%         %% x[i].  If a[n] is in F, then we succeed (ie. accept the string).
%         %% """
%         M = 1,
%         length(X,N),
%         N2 #= N+1,
%         length(A,N2),
%         A ins 1..Q,

%         X ins 1..S, %% """Do this in case it's a var."""

%         element(M,A,AM),
%         AM #= Q0,       %% Set a[0], initial state
        
%         %% MiniZinc's infamous matrix element
%         %%   a[i+1] = d[a[i], x[i]]
%         numlist(1,N,Is),
%         maplist(regular_loop(A,X,D),Is),
        
%         %% member(A[N2], F). %% """Check the final state is in F."""
%         %% A[N2] :: F.       %% """Check the final state is in F."""
%         element(N2,A,AN2),
%         element(_,F,AN2).


% /*

%   regular2/7 is the same as regular/6 but it also returns A, 
%   the state list. This might be handy for debugging or
%   explorations.


% */

% regular2(X, Q, S, D, Q0, F, A) :-

%         %% """
%         %% If x has index set m..n-1, then a[m] holds the initial state
%         %% (q0), and a[i+1] holds the state we're in after  processing
%         %% x[i].  If a[n] is in F, then we succeed (ie. accept the string).
%         %% """
%         M = 1,
%         length(X,N),
%         N2 #= N+1,
%         length(A,N2),
%         A ins 1..Q,

%         X ins 1..S, %% """Do this in case it's a var."""

%         element(M,A,AM),
%         AM #= Q0,       %% Set a[0], initial state
        
%         %% MiniZinc's infamous matrix element
%         %%   a[i+1] = d[a[i], x[i]]
%         numlist(1,N,Is),
%         maplist(regular_loop(A,X,D),Is),
        
%         %% member(A[N2], F). %% """Check the final state is in F."""
%         %% A[N2] :: F.       %% """Check the final state is in F."""
%         element(N2,A,AN2),
%         element(_,F,AN2).


% %%
% %% The matrix element loop
% %%   a[i+1] = d[a[i], x[i]]
% %%
% regular_loop(A,X,D,I) :-
%         element(I,A,AI),
%         element(I,X,XI),
%         I1 #= I+1,
%         element(I1,A,AI1),
%         matrix_element(D,AI,XI,AI1).


%
% DFA examples
%

% regexp 123*21
dfa(1,Transition,NStates,InputMax,InitialState,AcceptingStates) :-
        Transition = [
                      [2, 0, 0], % transitions from state 1: -2-> state 2
                      [0, 3, 0], % transitions from state 2: -3-> state 3 % 
                      [0, 4, 3], % transitions from state 3: -3-> state 3, -2-> state 4
                      [5, 0, 0], % transitions from state 4: -1-> state 5
                      [0, 0, 0]  % transitions from state 5: (none)
                     ],
        NStates = 5,
        InputMax = 3,
        InitialState = 1,
        AcceptingStates = [5].

%
%% This example is from
%% "Handbook of Constraint programming", page 180
%%
%%
%%    ^(c+|a+b+a+){n}$
%%    ^(3+|1+2+1+){n}$
%%
%% Or:  aa*bb* + cc*
%% 
%% "It accepts the strings 'aaabaa' and 'cc', but not 'aacbba'."
%% 
dfa(2,Transition,NStates,InputMax,InitialState,AcceptingStates) :-
        Transition = [ 
                     %% inputs
                     %% 1 2 3 
                       [2,0,5], % 1 start
                       [2,3,0], % 2 loop + next
                       [4,3,0], % 3 next + loop
                       [4,0,0], % 4 loop + end state
                       [0,0,5]  % 5 loop and end state
                     ],
        NStates = 5,
        InputMax = 3,
        AcceptingStates = [4,5],
        InitialState = 1.


%%
%% Example1 from Choco http://www.emn.fr/x-info/choco-solver/doku.php?id=regular
%% 
%% First example
%%        t.add(new Transition(0, 1, 1));
%%        t.add(new Transition(1, 1, 2));
%%        t.add(new Transition(2, 1, 3));
%% 
%%        t.add(new Transition(3, 3, 0));
%%        t.add(new Transition(0, 3, 0));
%% 
dfa(3,Transition,NStates,InputMax,InitialState,AcceptingStates) :-
        Transition = [ 
                       [2, 0, 1], %% 1 
                       [3, 0, 0], %% 2
                       [0, 4, 0], %% 3 
                       [0, 0, 1]  %% 4 
                     ],
        length(Transition,NStates),
        InputMax = 3,
        InitialState = 1,
        AcceptingStates = [4].


%%
%% Example2 from Choco http://www.emn.fr/x-info/choco-solver/doku.php?id=regular
%% 
%% This is an all_equal constraint. The initial_state decides which 
%% values it will be

dfa(4,Transition,NStates,InputMax,InitialState,AcceptingStates) :-
        Transition = [ 
                       [1, 0, 0, 0], %% 1 (start)
                       [0, 2, 0, 0], %% 2
                       [0, 0, 3, 0], %% 3 
                       [0, 0, 0, 4]  %% 4 
                     ],
        length(Transition,NStates),
        InputMax = 4,
        InitialState = 3,
        AcceptingStates = [1,2,3,4].

%%
%% Example3 from Choco http://www.emn.fr/x-info/choco-solver/doku.php?id=regular
%% 
%% This is stretchPath, not regular
%%  lgt.add(new int[]{2, 2, 2}); // stretches of value 2 are exactly of size 2
%%  lgt.add(new int[]{0, 2, 2}); // stretches of value 0 are exactly of size 2
%%  lgt.add(new int[]{1, 2, 3}); // stretches of value 1 are at least of size 2 and at most 3
%%
%% This should be interpreted as:
%%  0{2}1{2,3}2{2}
%% for all permutations of 0,1,and 2.
%% Or rather:
%%  1{2}2{2,3}3{2}
%% for all permutations of 1,2, and 3 since 0 is not allowed as an input value...
%%
dfa(5,Transition,NStates,InputMax,InitialState,AcceptingStates) :-
        Transition = [ 
                       [2, 4, 7], %% 1 (start)
                       [3, 0, 0], %% 2
                       [0, 4, 7], %% 3 (end)
                       [0, 5, 0], %% 4
                       [2, 6, 7], %% 5 (end)
                       [2, 0, 7], %% 6 (end)
                       [0, 0, 8], %% 7
                       [2, 4, 0]  %% 8 (end)
                     ],
        length(Transition,NStates),
        InputMax = 3,
        InitialState = 1,
        AcceptingStates = [3,5,6,8].


%%
%% All different.
%% This works in principle (here is n=3), but the automaton will 
%% be forbiddingly large for larger arrays.
%% 
dfa(6,Transition,NStates,InputMax,InitialState,AcceptingStates) :-
        Transition = [ 
                     %%%%  1   2   3
                       [ 2,  7, 12], %% 1 (start)
                       [ 0,  3,  5], %% 2
                       [ 0,  0,  4], %% 3
                       [ 0,  0,  0], %% 4
                       [ 0,  6,  0], %% 5
                       [ 0,  0,  0], %% 6
                       [ 8,  0, 10], %% 7
                       [ 0,  0,  9], %% 8
                       [ 0,  0,  0], %% 9
                       [11,  0,  0], %% 10
                       [ 0,  0,  0], %% 11
                       [13, 15,  0], %% 12
                       [ 0, 14,  0], %% 13
                       [ 0,  0,  0], %% 14
                       [ 16, 0,  0], %% 15
                       [ 0,  0,  0]  %% 16
                     ],
        length(Transition,NStates),
        InputMax = 3,
        InitialState = 1,
        AcceptingStates = [4,6,9,11,14,16].



%%
%% "Need Regular Expression for Finite Automata: Even number of 1s and Even number of 0s"
%% http://stackoverflow.com/questions/17420332/need-regular-expression-for-finite-automata-even-number-of-1s-and-even-number-o
%%  
%%     Q1 <- 1 -> Q2
%%     ^          ^
%%     |          |
%%     2          2
%%     |          |
%%     v          v
%%     Q4 <- 1 -> Q3
%% 
dfa(7,Transition,NStates,InputMax,InitialState,AcceptingStates) :-
        Transition = [ 
                                %%  1   2
                       [ 2,  4], %% state 1 Q1 start, final
                       [ 1,  3], %% state 2 Q2 
                       [ 4,  2], %% state 3 Q3
                       [ 3,  1]  %% state 4 Q4
                     ],
        length(Transition,NStates),
        InputMax = 2,
        InitialState = 1,
        AcceptingStates = [1].
