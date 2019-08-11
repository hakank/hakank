/*

  All interval problem in SWI Prolog

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 9,
        time2(findall(X,all_interval(N,X), L),Time),
        length(L, Len),
        format("Total: ~d Time: ~fs~n", [Len,Time]),
        nl.

% Time for first solution
go2 :-
        writeln("First solutions for N in 2..20"),
        N in 2..20,
        indomain(N),
        writeln(n=N),
        time2(all_interval(N,_X,true), Time),
        format("time: ~fs~n",[Time]),
        fail,
        nl.

go2.

% go3 :- 
%    all_interval(2000,X,[ff,split]),
%    writeln(X),
%    nl.


all_interval(N,X) :-
        all_interval(N,X,false).

all_interval(N,X,FirstSolution) :-

        length(X, N),
        X ins 1..N,
        
        N1 #= N-1,
        length(Diffs, N1),
        Diffs ins 1..N1,

        all_distinct(X),
        all_distinct(Diffs),

        
        % foreach(K in 1..N1)
        %   Diffs[K] #= abs(X[K+1] - X[K])
        % end,
        % findall(T, (between(1,N1,K), K1 #= K+1,
        %             element(K1,X,XK1),element(K,X,XK),
        %             T #= abs(XK1 - XK)), Diffs),

        x_diffs(X, Diffs),
        
        %% symmetry breaking
        element(1,X,X1),
        element(N1,X,XN1),
        X1 #< XN1,
        element(1,Diffs,Diffs1),
        element(2,Diffs,Diffs2),
        Diffs1 #< Diffs2,

        %% solve
        append(X, Diffs, Vars),
       

        (
         FirstSolution \= true
        -> 
         labeling([min,bisect], Vars),
         format("x: ~w  diffs: ~w~n", [X, Diffs])
        ;
         %% symmetry breaking for one solutions
         increasing(Diffs),
         once(labeling([min,bisect], Vars))
        ).


x_diffs(X,Diffs) :-
        length(Diffs,DiffsLen),
        numlist(1,DiffsLen,Is),
        x_diffs_(Is,X,Diffs).

x_diffs_([],_X,_Diffs).
x_diffs_([I|Is],X,Diffs) :-
        element(I,X,XK),
        I1 #= I+1,
        element(I1,X,XK1),
        Abs #= abs(XK-XK1),
        element(I,Diffs,Abs),
        x_diffs_(Is,X,Diffs).


