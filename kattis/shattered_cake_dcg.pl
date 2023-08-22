% https://open.kattis.com/problems/shatteredcake
% 6s
% 1.4 Easy

% DCG version:
% Nope, this version fails already on case 5/6 with a Memory Limit Exceeded
% so it's worse than shattered_cake.pl
% But it's at least quite shorter: 277chars vs 501chars (uncompressed)
% Later: In order to understand the code, I added newlines...
% I test to add garbage_collect to free some space. Nope, that didn't help.
:- use_module(library(dcg/basics)).
main :-
    % garbage_collect(),
    once(phrase_from_stream(p([W,L]),user_input)),
    sumlist(L,Tot),
    Len is Tot / W,
    format('~d~n',[Len]).
main.
q([AB|ABs]) --> integer(A)," ",integer(B),{AB is A*B},eol,q(ABs).
q([]) --> [].
p([W,L]) --> integer(W),eol,integer(_),eol,q(L).
