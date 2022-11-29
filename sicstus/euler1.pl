/*

  Euler #1 in SICStus Prolog

  http://projecteuler.net/index.php?section=problems&id=1
  """
  If we list all the natural numbers below 10 that are multiples of 3 
  or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  Note: euler1a..euler1e are port of my SWI Prolog programs.
        The rest is SICStus Prolog specific (or new).
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        %% time(euler1f), 
        run_problems([
                      % euler1a,
                      % euler1b,
                      % euler1c,
                      % euler1d,
                      % euler1e,
                      euler1f % ,
                      % euler1g,
                      % euler1h
                      ]),
        nl.


%%
%% 0.022s
%%
euler1a :-
        once(euler1a_pred(1, 999, 0, Res)),
        writeln(Res).

euler1a_pred(N, Limit, R, R) :- N #> Limit.
euler1a_pred(N, Limit, R, R2) :-
        domain([B],0,1),
        (N mod 3 #= 0 #\/N mod 5 #= 0) #<=> B #= 1,
        R1 #= R + B*N,
        N2 #= N + 1,
        euler1a_pred(N2, Limit, R1,R2).


%%
%% 0.001s
%%
euler1b :-
        numlist(1,999,Is),
        once(maplist(p_b,Is,Ls)),
        sum_list(Ls,Res),
        writeln(Res).

p_b(N,N) :- N mod 3 #= 0.
p_b(N,N) :- N mod 5 #= 0.
p_b(_,0).




%%
%% 0.007s
%%
euler1c :-
        setof(I,
                (between(1,999,I),
                 (I mod 3 #= 0 ; I mod 5 #= 0)
                ),
                Is
               ),
        sum_list(Is,Res),
        writeln(Res).

%%
%% 0.006s
%%
euler1d :-
        numlist(1,999,Is),
        include(p_d,Is,Ls),
        sum_list(Ls,Res),
        writeln(Res).

p_d(N) :- N mod 3 #= 0 ; N mod 5 #= 0.


%%
%% 0.003s
%%
euler1e :-
        numlist(1,999,Is),
        setof(I,
                (member(I,Is),
                 p_d(I)
                ),
                Ls
               ),
        sum_list(Ls,Res),
        writeln(Res).

%%
%% SICStus Prolog specific.
%% Using for/3 and foreach/2 and sumlist/2.
%% 0.001s
%%
euler1f :-
        (for(N,2,999),
         foreach(X,Vals) do
          ((N mod 3 =:= 0 ; N mod 5 =:= 0) ->
           X = N
          ;
           X = 0
          )
        ),
        sumlist(Vals,Sum),
        writeln(Sum).

%%
%% Using for/3 and fromto/4 (no sumlist/2 needed)
%%
%% 0.001s
%%
euler1g :-
        (for(N,2,999),
         fromto(0,In,Out,Sum) do
          ((N mod 3 =:= 0 ; N mod 5 =:= 0) ->
           Out is In + N
          ;
           Out is In
          )
        ),
        writeln(Sum).

%%
%% From my ECLiPse program euler1.ecl
%% 0.000s
euler1h :-
        numlist(1,999,L),
        filter(div3_or_5, L, Includes),
        sumlist(Includes, SumList),
        writeln(SumList).

div3_or_5(N) :- 0 is N mod 3 ;  0 is N mod 5.



