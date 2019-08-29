/*

  General alphametic (cryptarithmetic) solver in SWI Prolog
  
  This is a fairly general solver for alphametic problems, 
  but it requires explicit variables. E.g.

    alphametic([[S,E,N,D],[M,O,R,E],[M,O,N,E,Y]], Base, Res)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        problem(P,Ss,Base,Ls),
        writeln(p=P),
        writeln(Ss),
        writeln(Ls),
        nl,
        once(alphametic(Ss,Ls,Base, Vars)),
        writeln(vars=Vars),
        maplist(writeln,Ls),
        nl,
        fail,
        nl.

go.

%%
%% alphametic(S, L,Base, Vars)
%%
alphametic(S, L,Base, Vars) :-
        reverse(L,Rev),
        Rev = [Last|Sums],
        term_variables(L,Vars),
        Base1 #= Base-1,
        Vars ins 0..Base1,
        all_different(Vars),
        calc_vals(Sums,Base,0,Vals),
        calc(Last,Base,Vals),
        maplist(first_larger_than_0,Sums),
                
        labeling([ff], Vars).

first_larger_than_0(Ss) :-
        element(1,Ss,S),
        S #> 0.

calc_vals([],_Base,Vals,Vals).
calc_vals([S|Ss],Base,Vals0,Vals) :-
        calc(S,Base,Val),
        Vals1 #= Vals0 + Val,
        calc_vals(Ss,Base,Vals1,Vals).

calc(X,Base, Y) :-
        length(X,Len),
        numlist(1,Len,Is),
        calc_(Is,X,Base,Len,0,Sum),
        Sum #= Y.

calc_([],_X,_Base,_Len,Sum,Sum).
calc_([I|Is],[X|Xs],Base,Len,Sum0,Sum) :-
        LenI #= Len-I,
        Sum1 #= Sum0 + X*Base^LenI,
        calc_(Is,Xs,Base,Len,Sum1,Sum).
        

print_res(L) :-
        reverse(L,Rev),
        Rev = [Last|Sums],
        reverse(Sums,Sums2),
        maplist(print_single,Sums2),
        print_single(Last),
        nl.

print_single(L) :-
        maplist(write,L),
        nl.



problem(1,Ss,Base,Ls) :-
        Ss = "SEND+MORE=MONEY",
        Base = 10,
        Ls = [[_S,E,N,_D],[M,O,_R,E],[M,O,N,E,_Y]].


problem(2,Ss,Base,Ls) :-
        Ss = "SATURN+URANUS+NEPTUNE+PLUTO=PLANETS",        
        Base = 10,
        Ls = [[S,A,T,U,R,N], 
              [U,R,A,N,U,S], 
              [N,E,P,T,U,N,E],
              [P,L,U,T,_O],    
              [P,L,A,N,E,T,S]].

problem(3,Ss,Base,Ls) :-
        Ss = "VINGT+CINQ+CINQ=TRENTE",
        Base = 10,
        Ls = [[V,I,N,G,T],[C,I,N,Q],[C,I,N,Q],[T,R,E,N,T,E]].

problem(4,Ss,Base,Ls) :-
        Ss = "EIN+EIN+EIN+EIN=VIER",
        Base = 10,
        Ls = [[E,I,N],[E,I,N],[E,I,N],[E,I,N],[V,I,E,R]].

problem(5,Ss,Base,Ls) :-
        Ss = "WRONG+WRONG=RIGHT",
        Base = 10,
        Ls = [[W,R,O,N,G],[W,R,O,N,G],[R,I,G,H,T]].


