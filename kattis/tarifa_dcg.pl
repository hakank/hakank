% https://open.kattis.com/problems/tarifa
% 1s
% 1.4 Easy

% Using DCG.
% Shorter (250 chars), but not as short as Andriy Zyevakov's 213 chars!
:- use_module(library(dcg/basics)).
main:-phrase_from_stream(p([Max,Num,Ns]),user_input),sum_list(Ns,Sum),X is Max*Num-Sum+Max,writeln(X).
main.
q([N|Ns])-->integer(N),eol,q(Ns).
q([])-->[].
p([Max,Num,Ns])-->integer(Max),eol,integer(Num),eol,q(Ns).
