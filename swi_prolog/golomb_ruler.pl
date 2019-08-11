/*

  Golomb ruler in SWI Prolog

  A Golomb ruler is a set of integers (marks) a(1) < ...  < a(n) such
  that all the differences a(i)-a(j) (i > j) are distinct.  Clearly we
  may assume a(1)=0.  Then a(n) is the length of the Golomb ruler.
  For a given number of marks, n, we are interested in finding the
  shortest Golomb rulers.  Such rulers are called optimal. 

  See http://www.research.ibm.com/people/s/shearer/grule.html


  Benchmark for N=8. Clearly golomb2/2 is much faster. And much more elegant.

  golomb/2
  n=8
  % 175,472,752 inferences, 9.093 CPU in 9.094 seconds (100% CPU, 19297559 Lips)
  [0,1,4,9,15,22,32,34]


  golomb2/2
  % 33,134,620 inferences, 1.998 CPU in 1.998 seconds (100% CPU, 16580421 Lips)
  [0,1,4,9,15,22,32,34]


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        time(once(golomb(6, Xs))),
        writeln(Xs),
        nl.

go2 :-
        time(once(golomb2(8, Xs))),
        writeln(Xs),
        nl.

%% Benchmark
go3 :-
        writeln("golomb/2"),
        time(once(golomb(8, Xs1))),
        writeln(Xs1),
        nl,
        writeln("golomb2/2"),
        time(once(golomb2(8, Xs2))),
        writeln(Xs2),
        nl.


go4 :- 
        between(2,10,N),
        writeln(n=N),
        time(once(golomb2(N,Xs))),
        writeln(Xs),
        nl,
        fail,
        nl.

go4.

%%
%% Port of my Picat model.
%%
golomb(N, Xs) :-
        writeln(n=N),
        length(Xs,N),
        NN #= 2^(N-1)-1,
        Xs ins 0..NN,
        append([Xs1,Xs2|_],[XsN1,XsN], Xs),
        
        %% n #= Xs[N], %% to minimize
        Xs1 #= 0,

        all_different(Xs),
        %% all_distinct(Xs),        
        increasing_strict(Xs),

        %% Diffs
        findall([I,J],
                (between(1,N,I),
                 between(1,N,J),
                 I #\= J
                ),
                IJs),
        maplist(diffs1(Xs),IJs,Diffs),
        all_different(Diffs),

        %% Symmetry breaking
        Xs2 - Xs1 #< XsN - XsN1,
        append([Diffs1|_],[DiffsN],Diffs),
        Diffs1 #< DiffsN,

        flatten([Xs],Vars),
        labeling([min(XsN),ff,enum],Vars).

diffs1(Xs,[I,J],Diff) :-
        element(I,Xs,XsI),
        element(J,Xs,XsJ),
        Diff #= XsI - XsJ.

%%
%% From an ECLiPSe model.
%% Much faster (and more elegant).
%%
golomb2(N, Xs) :-
        length(Xs, N),
        NN #= 2^(N-1)-1,
        Xs ins 0..NN,
        append([0|_], [Xn], Xs),
        increasing(Xs),
        distances(Xs, Diffs),
        Diffs ins 1..NN,
        all_different(Diffs),
        append([D1|_], [Dn], Diffs),
        D1 #< Dn,

        labeling([min(Xn),enum],Xs).

distances([], []).
distances([X|Ys], D0) :-
        distances(X, Ys, D0, D1),
        distances(Ys, D1).

distances(_, [], D, D).
distances(X, [Y|Ys], [Diff|D1], D0) :-
        Diff #= Y-X,
        distances(X, Ys, D1, D0).
