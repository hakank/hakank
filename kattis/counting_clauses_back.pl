% https://open.kattis.com/problems/countingclauses
% 1s
% 1.6 Easy

% I misread the description and tried to implement a proper 3-SAT solver.
% (It does not work.)
% The problem is - as hinted in the name - just to count the number of
% clauses.
% Sigh...

:- use_module(library(clpfd)).
main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",[NV|Ss]),
    split_string(NV," ","", NVs),
    maplist(number_string,[N,NumVars],NVs),
    writeln(n=N),
    writeln(num_vars=NumVars),    
    writeln(ss=Ss),
    length(X,NumVars),
    X ins 0..1,
    s(Ss,X,Rs),
    writeln(rs=Rs),
    conj(Rs,Conj),
    writeln(conj=Conj),
    Conj,
    (labeling([ff,bisect],X) -> writeln(satisfactory=X) ; writeln(unsatisfactory)),
    fail.
main.

s([],_,[]).
s([L|Ls],X,[Ds|Rs]) :-
    nl,
    writeln(l=L),
    writeln(x=X),
    split_string(L," ","",Nss),
    writeln(ns0=Nss),
    maplist(number_string,Ns,Nss),
    writeln(ns=Ns),
    t(Ns,V),
    writeln(v=V),
    t2(V,X,R),
    writeln(r=R),    
    disj(R,Ds),
    writeln(ds=Ds),
    s(Ls,X,Rs).

t([],[]).
t([D|Ds],[V|S]) :-
    (D < 0 ->
        V = 0
    ;
        V = 1
    ),
    t(Ds,S).
    
t2([],[],[]).
t2([D|Ds],[X|Xs],[D#=X|R]) :-
    t2(Ds,Xs,R).

disj([D|Ds],Disj) :-
        foldl(disj1,Ds,D,Disj).
disj1(A,B,C) :-C = #\/(B,A).

conj([D|Ds],Disj) :-
        foldl(conj1,Ds,D,Disj).
conj1(A,B,C) :-C = #/\(B,A).



