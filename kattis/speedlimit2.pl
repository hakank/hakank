% https://open.kattis.com/problems/speedlimit
% Time limit: 1s
% Diff 1.5 Easy

% Using read_string/3.
% It's quite shorter than speedlimit.pl and just a little shorter than speedlimit_dcg.pl

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s(["-1"]).
s([S|Ss]) :-
    number_string(N,S),
    length(L1,N),
    append(L1,Rest,Ss),
    findall([A,B],(member(AB,L1),split_string(AB," ","",AB0),maplist(number_string,[A,B],AB0)),L),
    zip2(Speeds,Times,L),
    diffs(Times,Diffs),
    scalar_product2(Diffs,Speeds,Tot),
    format('~d miles~n',[Tot]),
    s(Rest).
diffs(L,Diffs) :-
    append([0],L,L2),
    findall(Diff,(nextto(X,Y,L2), Diff is Y-X),Diffs).
scalar_product2(L1,L2,Res) :-
    scalar_product2(L1,L2,0,Res).
scalar_product2([],[],Res,Res).
scalar_product2([X|Xs],[Y|Ys],Res0,Res) :-
    Res1 is Res0 + X*Y,
    scalar_product2(Xs,Ys,Res1,Res).
zip2(L1,L2,L) :-
        zip2(L1,L2,[],L).
zip2([],[], L,L).
zip2([H1|T1],[H2|T2], L0,[L1|L]) :-
        L1 = [H1,H2],
        zip2(T1,T2, L0,L).
