% https://open.kattis.com/problems/dicecup
% 1s
% 1.4 Easy

% Andriy Zyevakov has a solution with 176 chars!
% Using DCG: Longer, 437 chars.
:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p([A,B]),user_input),
    findall(Sm,(between(1,A,I),between(1,B,J),Sm is I+J),C),
    msort(C,Cs),clumped(Cs,Cl),sort(2,@>=,Cl,Srt),[_-Max|_]=Srt,
    findall(K,(member(K-Max,Srt)),Ks),sort(Ks,KS),maplist(writeln,KS).
main.
p([A,B]) --> integer(A)," ", integer(B),eol.