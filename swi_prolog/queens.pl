/*

  N-Queens problem in SWI Prolog

  See https://en.wikipedia.org/wiki/Eight_queens_puzzle

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 8,
        time(findall(Q, queens1(N,Q),L)),
        length(L, Len),
        writeln(L),
        writeln(len=Len),
        nl.

%
% [n=8,queens1]
% [1,5,8,6,3,7,2,4]
% N=8 Time 0.04s
%
% [n=12,queens1]
% [1,3,5,11,8,10,12,4,2,7,9,6]
% N=12 Time 0.13s
%
% [n=20,queens1]
% [1,3,5,14,17,4,16,7,12,18,15,19,6,10,20,11,8,2,13,9]
% N=20 Time 0.58s
%
% [n=50,queens1]
% [1,3,5,22,41,4,34,7,33,42,49,46,6,31,36,28,8,29,35,30,27,14,9,37,32,13,47,50,24,10,45,40,48,39,44,2,19,11,43,15,25,38,20,23,26,16,12,17,21,18]
% N=50 Time 21.74s
%
% [n=100,queens1]
% [1,3,5,57,59,4,64,7,58,71,81,60,6,91,82,90,8,83,77,65,73,26,9,45,37,63,66,62,44,10,48,54,43,69,42,47,18,11,72,68,50,56,61,36,33,17,12,51,100,93,97,88,35,84,78,19,13,99,67,76,92,75,87,96,94,85,20,14,95,32,98,55,40,80,49,52,46,53,21,15,41,2,27,34,22,70,74,29,25,30,38,86,16,79,24,39,28,23,31,89]
% N=100 Time 136.06s
%
% [skipping the rest of Ns]
go2 :-
        test_queens([8,12,20,50,100,200,500,1000],queens1),
        nl.

go3 :-
        N = 8,
        % time(once(queens3(N,Q))),
        % writeln(Q),
        time(findall(Q, queens1(N,Q),L)),
        length(L, Len),
        writeln(L),
        writeln(len=Len),
        nl.


go4 :-
        test_queens([8,12,20,50,100,200,500,1000],queens3),
        nl.

%%
%% compare queens3 and queens1 (queens3 is much faster)
%%
go5 :-
        member(P,[queens3,queens1]),
        test_queens([8,12,20,50,100], P),
        fail,
        nl.


queens1(N, Q) :- 
        length(Q, N),
        Q ins 1..N,
        findall([I,J], (between(1,N,I), between(1,N, J), I < J), Ixs),
        queens1_(Ixs, Q),
        labeling([ffc,enum], Q).

queens1_([],_Q).
queens1_([[I,J]|IJs],Q) :-
        element(I,Q,QI),
        element(J,Q,QJ),
        QI #\= QJ,
        QI + I #\= QJ + J,
        QI - I #\= QJ - J,
        queens1_(IJs, Q).



%%
%% Using all_distinct/1. Much faster than queens1/2.
%% 
queens3(N, Q) :-
        length(Q, N), 
        Q ins 1..N,
        all_distinct(Q),
        queens3_(Q,1,[],Q1,[],Q2),
        all_distinct(Q1),
        all_distinct(Q2),
        labeling([ffc,enum],Q).

queens3_([],_I,Q1,Q1,Q2,Q2).
queens3_([Q|Qs],I, Q10,[Qminus|Q1],Q20,[Qplus|Q2]) :-
        Qplus #= Q+I,
        Qminus #= Q-I,
        I2 #= I+1,
        queens3_(Qs,I2, Q10,Q1,Q20,Q2).


%%
%% Using findall/3 don't work. 
%%
queens3_findall(N, Q) :-
        length(Q, N), 
        Q ins 1..N,
        NegN #= - N,
        Domain = NegN..N,
        writeln(domain=Domain),
        all_distinct(Q),
        % all_different([$Q[I]-I : I in 1..N]),
        findall(QI1, (between(1,N,I), element(I,Q,QI), QI1#=QI-I), L1),
        
        % L1 ins NegN..N,        
        writeln(l1=L1),        
        print_attrs_list(L1),
        list_domains(L1,L1Domains),
        writeln(l1Domains=L1Domains),
        all_distinct(L1),
        
        % all_different([$Q[I]+I : I in 1..N]),
        findall(QI2, (between(1,N,I), element(I,Q,QI), QI2#=QI+I), L2),
        % L2 ins NegN..N,
        print_attrs_list(L2),
        list_domains(L2,L2Domains),
        writeln(l2Domains=L2Domains),
        all_distinct(L2),
        writeln(L2),

        % Q = [2,4,1,3],
        % append([L1,L2,Q], Vars), % This yields a hugs number of (identical) solutions
        % labeling([],Vars).
        labeling([],Q).

%%
%% Test: Using all_distinct/1 and maplist. Nope, don't work either...
%%
queens2(N, Q) :-
        length(Q, N), 
        Q ins 1..N,
        all_distinct(Q),
        
        numlist(1,N,Is),
        findall(1,between(1,N,_),Ones),
        maplist(q_plus(Q),Is,Ones, Plus),
        all_distinct(Plus),
        
        findall(-1,between(1,N,_),NegativeOnes),
        maplist(q_plus(Q),Is,NegativeOnes, Minus),
        all_distinct(Minus),
        append([Q,Plus,Minus],Vars),
        % append([Q],Vars),
        labeling([ff],Vars).

q_plus(Q,I,Add,Y) :-
        writeln(q_plus(Q,I,Add,Y)),
        element(I,Q,QI),
        Y #= QI + Add.


%%
%% queens(List, QueensPredicate).
%%
%% Test a queens predicate on a list of sizes.
%%
test_queens([], _P).
test_queens([N|Ns], P) :-
        nl,nl,
        writeln([n=N,P]),
        time2(once(call(P, N,Q)),Time),
        writeln(Q),
        format("N=~d Time ~2fs", [N,Time]),
        test_queens(Ns,P).
