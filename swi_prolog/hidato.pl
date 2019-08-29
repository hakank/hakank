/*

  Hidato puzzle in SWI Prolog

  http://www.shockwave.com/gamelanding/hidato.jsp
  http://www.hidato.com/
 
  """
  Puzzles start semi-filled with numbered tiles.
  The first and last numbers are circled.
  Connect the numbers together to win. Consecutive
  number must touch horizontally, vertically, or
  diagonally.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        time(solveit(6)).

go1 :- 
        time(solveit(1)).

go2 :-
        member(P,[1,2,3,4,5,6,8]),
        time(solveit(P)),
        fail,
        nl.
go2.

%%
%% Problem 7 is hard!
%%
go3 :- 
        time(solveit(7)),
        nl.


%%
%% Benchmark all combinations of strategies for problem 6
%%
%% These are the only combinations that solves problem 6 under 3s.
%% It seems that it's leftmost+up (i.e. the default) that's the best.
%% 
%% [leftmost,up,step]
%% % 36,839,745 inferences, 2.302 CPU in 2.302 seconds (100% CPU, 16004092 Lips)
%%
%% [leftmost,up,enum]
%% % 36,808,741 inferences, 2.287 CPU in 2.287 seconds (100% CPU, 16095059 Lips)
%%
%% [leftmost,up,bisect]
%% % 36,857,577 inferences, 2.286 CPU in 2.286 seconds (100% CPU, 16125705 Lips)
%%
go4 :-

        variable_selection(Vars),
        value_selection(Vals),
        strategy_selection(Strategies),
        member(Var,Vars),
        member(Val,Vals),
        member(Strategy,Strategies),
        writeln([Var,Val,Strategy]),
        Timeout #= 3,          % timeout (seconds)
        problem(6,X),
        catch(call_with_time_limit(Timeout,
                                   time(once(hidato(X,[Var,Val,Strategy])))
                                   ),
              time_limit_exceeded,
              Result = timeout),
        writeln(result=Result),
        nl,
        fail,
        nl.

go4.
        

%%
%% Wrapper
%%
solveit(Problem) :-
        format("\nProblem ~d\n", [Problem]),
        problem(Problem,X),
        once(hidato(X)),
        pretty_print(X).

%%
%% Place all integers from 1..Rows*Cols
%%
hidato(X) :-
        hidato(X,[]).
hidato(X, Label) :-

   length(X,N),
   flatten(X,XList),
   NN #= N*N,
   XList ins 1..NN,

   % Valid connections, used by the table constraint below
   findall([I1,J1,I2,J2],
           (between(1,N,I1),between(1,N,J1),
            between(1,N,I2),between(1,N,J2),
            abs(I1-I2) #=< 1,
            abs(J1-J2) #=< 1,
            (I1 #\=I2 ; J1 #\= J2)
           ),
           Connections),

   all_different(XList),

   NN1 #= (NN)-1,
   numlist(1,NN1,Ks),
   maplist(hidato_loop(XList,N), Ks,Extra,Connect),

   tuples_in(Connect,Connections),
   
   %% search
   flatten([Connect,XList,Extra],Vars),
   labeling(Label,Vars).


%%
%% The hidato loop (foreach K in 1..(N*N)-1)
%%
%% Extra and Connect collects the temporary variables.
%% 
hidato_loop(XList,N, K,Extra,Connect) :-
        %% define temporary variables for finding
        %% the index of this and the next number (K)
        I in 1..N,              % index I
        J in 1..N,              % index J
        A in -1..1,             % offset from K's position
        B in -1..1,             % ibid
        
        %% needed for element
        IA in 1..N,
        JB in 1..N,
        IA #= I+A,
        JB #= J+B,
        
        %% some extra constraints
        abs(A)+abs(B) #>= 1, % both A and B cannot be 0, i.e. it must be a move

        %% 1. First: fix this k, i.e.
        %% K #= X[I,J]
        I1 #= I-1,
        IJ #= (I1)*N + J,
        element(IJ, XList, K),
       
        %% 2. Then, find the position of the next value, i.e.
        %% K+1 #= X[I+A,J+B], % this don't work
        IA_JB #= (I-1+A)*N + JB,
        K1 #= K+1,
        element(IA_JB, XList, K1),
        Extra = [IJ,IA_JB,A,B],
        Connect= [I,J,IA,JB].


pretty_print(X) :-
        maplist(writeln,X),
        nl.


%
% Problems
%


% Simple problem
%
% solution:
%   6 7 9
%   5 2 8
%   1 4 3
% 
problem(1, P) :- 
    P = [[6,_,9],
         [_,2,8],
         [1,_,_]].



problem(2, P) :- 
    P = [[ _,44,41, _, _, _, _],
         [ _,43, _,28,29, _, _],
         [ _, 1, _, _, _,33, _],
         [ _, 2,25, 4,34, _,36],
         [49,16, _,23, _, _, _],
         [ _,19, _, _,12, 7, _],
         [ _, _, _,14, _, _, _]]. 



% Problems from the book:
% Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

% problem 1 (Practice]
problem(3, P) :- 
    P = [[_, _,20, _, _],
         [_, _, _,16,18],
         [22, _,15, _, _],
         [23, _, 1,14,11],
         [_,25, _, _,12]].
         


% problem 2 (Practice]
problem(4, P) :- 
    P = [[_, _, _, _,14],
         [_,18,12, _, _],
         [_, _,17, 4, 5],
         [_, _, 7, _, _],
         [9, 8,25, 1, _]].


% problem 3 (Beginner]
problem(5, P) :- 
    P = [[ _,26, _, _, _,18],
         [ _, _,27, _, _,19],
         [31,23, _, _,14, _],
         [ _,33, 8, _,15, 1],
         [ _, _, _, 5, _, _],
         [35,36, _,10, _, _]].



% Problem 15 (Intermediate]
problem(6,P) :- 
   P = [[64, _, _, _, _, _, _, _],
        [ 1,63, _,59,15,57,53, _],
        [ _, 4, _,14, _, _, _, _],
        [ 3, _,11, _,20,19, _,50],
        [ _, _, _, _,22, _,48,40],
        [ 9, _, _,32,23, _, _,41],
        [27, _, _, _,36, _,46, _],
        [28,30, _,35, _, _, _, _]].


% Problem 156 (Master]
% (This is harder to solve than the 12x12 prolem 188 below...]
problem(7, P) :- 
    P = [[88, _, _,100, _, _,37,_, _,34],
         [ _,86, _,96,41, _, _,36, _, _],
         [ _,93,95,83, _, _, _,31,47, _],
         [ _,91, _, _, _, _, _,29, _, _],
         [11, _, _, _, _, _, _,45,51, _],
         [ _, 9, 5, 3, 1, _, _, _, _, _],
         [ _,13, 4, _, _, _, _, _, _, _],
         [15, _, _,25, _, _,54,67, _, _],
         [ _,17, _,23, _,60,59, _,69, _],
         [19, _,21,62,63, _, _, _, _, _]].


% Problem 188 (Genius]
problem(8, P) :- 
    P = [[  _,  _,134,  2,  4,  _,  _,  _,  _,  _,  _,  _],
         [136,  _,  _,  1,  _,  5,  6, 10,115,106,  _,  _],
         [139,  _,  _,124,  _,122,117,  _,  _,107,  _,  _],
         [  _,131,126,  _,123,  _,  _, 12,  _,  _,  _,103],
         [  _,  _,144,  _,  _,  _,  _,  _, 14,  _, 99,101],
         [  _,  _,129,  _, 23, 21,  _, 16, 65, 97, 96,  _],
         [ 30, 29, 25,  _,  _, 19,  _,  _,  _, 66, 94,  _],
         [ 32,  _,  _, 27, 57, 59, 60,  _,  _,  _,  _, 92],
         [  _, 40, 42,  _, 56, 58,  _,  _, 72,  _,  _,  _],
         [  _, 39,  _,  _,  _,  _, 78, 73, 71, 85, 69,  _],
         [ 35,  _,  _, 46, 53,  _,  _,  _, 80, 84,  _,  _],
         [ 36,  _, 45,  _,  _, 52, 51,  _,  _,  _,  _, 88]].
