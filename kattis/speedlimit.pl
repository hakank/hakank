% https://open.kattis.com/problems/speedlimit
% Time limit: 1s
% Diff 1.5 Easy

:- use_module(library(dcg/basics)).
% :- use_module(library(clpfd)).
main :-
    collect_lines_codes(S0),    
    flatten(S0,S1),
    % remove last -1 line
    append(S,[45,49,10],S1),
    once(read_all(Groups,S,[])),
    check_groups(Groups).
main.

diffs(L,Diffs) :-
    append([0],L,L2),
    findall(Diff,(nextto(X,Y,L2), Diff is Y-X),Diffs).


check_groups([]).
check_groups([Group|Groups]) :-
    zip2(Speeds,Times,Group),
    diffs(Times,Diffs),
    % scalar_product(Diffs,Speeds,#=,Tot),
    scalar_product2(Diffs,Speeds,Tot),
    format('~d miles~n',[Tot]),
    check_groups(Groups).

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


% Input as a single string
collect_lines_codes(Lines) :-
    read_code(X),
    collect_lines_codes(X,[],Lines).

collect_lines_codes(X,Lines,Lines) :-
    X == end_of_file, !.
% Note: We have to add '\n (10) manually.
collect_lines_codes(X,Lines0,[X,[10]|Lines]) :-
    read_code(X2),
    collect_lines_codes(X2,Lines0,Lines).

read_code(S) :-
    read_line_to_codes(user_input,S).

%
% DCG
%
read_group([[Speed,Time]|Groups]) --> integer(Speed), " ", integer(Time), "\n", read_group(Groups).
read_group([[Speed,Time]])          --> integer(Speed), " ", integer(Time).
read_group([])                    --> [].

read_all([Group|Groups]) --> integer(_), "\n", read_group(Group), "\n", read_all(Groups).
read_all([Group])        --> integer(_), "\n", read_group(Group).
read_all([])             --> [].