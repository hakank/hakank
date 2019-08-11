/*

  (Decomposition of) global constraint circuit in SWI Prolog

  See Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html

  Also, see 
  https://www.swi-prolog.org/pldoc/man?predicate=circuit/1
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-   
        % find all circuits of order 5
        N = 5,
        Print = 1,
        test1(N, Print), % circuit_me/1
        test2(N, Print). % circuit/1 (built-in)


%%
%% Benchmark: circuit_me/1 vs circuit/1 (built-in)
%%
%% Show all circuits of order 4..11 and compare
%% circuit_me/1 (test1/2) with the built-in
%% circuit/1 (test2(2). The built-in surprisingly
%% a little slower. Perhaps there's a better branching
%% to use in test2/2?
%%
%% N: 4
%% circuit_me/1: 0.01s
%% circuit/1: 0.00s
%%
%% N: 5
%% circuit_me/1: 0.01s
%% circuit/1: 0.01s
%%
%% N: 6
%% circuit_me/1: 0.03s
%% circuit/1: 0.03s
%%
%% N: 7
%% circuit_me/1: 0.17s
%% circuit/1: 0.22s
%%
%% N: 8
%% circuit_me/1: 1.23s
%% circuit/1: 1.70s
%%
%% N: 9
%% circuit_me/1: 10.36s
%% circuit/1: 14.76s
%%
%% N: 10
%% circuit_me/1: 98.37s
%% circuit/1: 144.11s
%%
%% N: 11
%% circuit_me/1: ERROR: Stack limit (1.0Gb) exceeded
%%
%%
go2 :-
        Print = 0,
        between(4,11,N),
        format("\nN: ~d\n", [N]),
        time2(test1(N, Print), Time1),
        format("circuit_me/1: ~2fs~n",[Time1]),
        time2(test2(N, Print), Time2),
        format("circuit/1: ~2fs~n",[Time2]),
        fail,
        nl.

go2.

%
%% Benchmark: time to first solution for N=20.20..10
%%
%% Here we see that the built-in is much faster than circuit_me/1.
%%
%% N: 20
%% circuit_me/1: 0.10s
%% circuit/1: 0.02s
%%
%% N: 40
%% circuit_me/1: 0.54s
%% circuit/1: 0.11s
%%
%% N: 60
%% circuit_me/1: 1.65s
%% circuit/1: 0.73s
%%
%% N: 80
%% circuit_me/1: 4.11s
%% circuit/1: 1.61s
%%
%% N: 100
%% circuit_me/1: 8.77s
%% circuit/1: 2.45s
%%
%%
go3 :-
        between(0,20,100,N),
        format("N: ~d~n",[N]),
        
        length(X1,N),
        X1 ins 1..N,
        time2(once((circuit_me(X1),labeling([ff,enum],X1))),Time1),
        % writeln(x1=X1),
        format("circuit_me/1: ~2fs~n",[Time1]),
        
        length(X2,N),
        X2 ins 1..N,
        time2(once((circuit(X2),labeling([ff,enum],X2))),Time2),
        % writeln(x2=X2),
        format("circuit/1: ~2fs~n",[Time2]),
        nl,
        fail,
        nl.

go3.

%%
%% Testing circuit_path/2
%%
go4 :- 
   N = 6,
   length(X,N),
   X ins 1..N,
   length(Path,N),
   Path ins 1..N,
   circuit_path(X,Path),

   flatten([X,Path],Vars),
   labeling([],Vars),

   writeln('x   '=X),
   writeln(path=Path),
   nl,
   fail,
   nl.  

go4.


%%
%% Generate all circuits for N using my circuit_me/1
%%
test1(N,Print) :-
        length(X,N),
        X ins 1..N, 
        findall(X, (circuit_me(X),labeling([ffc,enum],X)),L),
        length(L,Len),
        (
         Print == 1
        ->
         writeln(L),
         writeln(len=Len)
        ;
         true
        ).

%%
%% Generate all circuits for N using my circuit/1 (built-in)
%%
test2(N,Print) :-
        length(X,N),
        X ins 1..N, 
        findall(X, (circuit(X),labeling([min,enum],X)),L),
        length(L,Len),
        (
         Print == 1
        ->
         writeln(L),
         writeln(len=Len)
        ;
         true
        ).


   
       
%
% circuit(X) succeeds for the array X if it's a circuit.
%
% This implementation use an extra array (Z) for the orbit of x[1].
%
circuit_me(X) :-
        
   length(X,N),
   length(Z,N),
   Z ins 1..N,

   %
   % The main constraint is that Z[I] must not be 1 
   % until I = N, and for I = N it must be 1.
   %
   all_different(X),
   all_different(Z),
   % all_distinct(X), % slower
   % all_distinct(Z), % slower

   % put the orbit of x[1] in in z[1..n]
   element(1,X,X1),
   element(1,Z,Z1),
   X1 #= Z1,
   
   % when I = N it must be 1
   element(N,Z,ZN),
   ZN #= 1,

   %
   % Get the orbit for Z.
   %
   numlist(2,N,Is),
   maplist(orbit(X,Z),Is).
   

%% foreach(I in 2..N)
%%   element(Z[I-1],X,Z[I])
%% end.  
orbit(X,Z,I) :-
        I1 #= I-1,
        element(I1,Z,ZI1),
        element(I,Z,ZI),
        element(ZI1,X,ZI).

%%
%% As circuit/1 but with the path as second parameter.
%%
circuit_path(X,Z) :-
   length(X,N),
   length(Z,N),
   Z ins 1..N,

      %
   % The main constraint is that Z[I] must not be 1 
   % until I = N, and for I = N it must be 1.
   %
   all_different(X),
   all_different(Z),
   % all_distinct(X), % slower
   % all_distinct(Z), % slower

   % put the orbit of x[1] in in z[1..n]
   element(1,X,X1),
   element(1,Z,Z1),
   X1 #= Z1,
   
   % when I = N it must be 1
   element(N,Z,ZN),
   ZN #= 1,

   %
   % Get the orbit for Z.
   %
   numlist(2,N,Is),
   maplist(orbit(X,Z),Is).
