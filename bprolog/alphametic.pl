/*

  General alphametic (cryptarithmetic) solver in B-Prolog.

  This is a fairly general solver, but it requires 
  explicit variables. E.g.

    alphametic([[S,E,N,D],[M,O,R,E],[M,O,N,E,Y]], Base, Res)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).


go :-
        time2(go1),
        time2(go2),
        time2(go3).

go1 :-
        Base = 10,
        % SEND + MORE = MONEY
        L = [[_S,E,N,_D],[M,O,_R,E],[M,O,N,E,_Y]],
        alphametic(L, Base, Res),
        writeln(Res),
        writeln(L),
        print_res(L).


go2 :- 
        Base = 10,
        % DONALD + GERALD = ROBERT
        Problem = [[D,O,_N,A,L,D],[_G,E,R,A,L,D],[R,O,_B,E,R,_T]],
        alphametic(Problem, Base, Res),
        writeln(Res),
        writeln(Problem),
        print_res(Problem).

go3 :-
        Base = 10,
        % SATURN+URANUS+NEPTUNE+PLUTO=PLANETS
        Problem = [[S,A,T,U,R,N], 
                    [U,R,A,N,U,S], 
                    [N,E,P,T,U,N,E],
                    [P,L,U,T,_O],    
                    [P,L,A,N,E,T,S]],
        alphametic(Problem, Base, Res),
        writeln(Res),
        writeln(Problem),
        print_res(Problem).

        

alphametic(L,Base, Vars) :- 

        reverse(L,Rev),
        Rev = [Last|Sums],

        term_variables(L, Vars),
        Vars :: 0..Base-1,

        alldifferent(Vars),
        Vals #= sum([Val : S in Sums,[Val],calc(S,Base,Val)]),
        calc(Last,Base,Vals),
        % foreach(S in Sums,S[1]#>0), % not allowed in v8
        foreach(I in 1..Sums^length, Sums[I,1]#>0),
        labeling([ff,split], Vars).

calc(X,Base,Y) :-
        length(X,Len),
        Y #= sum([X[I]*Base**(Len-I) : I in 1..Len]).


print_res(L) :-
        reverse(L,Rev),
        Rev = [Last|Sums],
        reverse(Sums,Sums2),
        foreach(S in Sums2, print_single(S)),
        print_single(Last),
        nl.

print_single(L) :-
        foreach(S in L, format("~d",[S])),
        nl.
