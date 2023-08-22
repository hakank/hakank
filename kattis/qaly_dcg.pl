% https://open.kattis.com/problems/qaly
% Time limit: 1s
% Difficulty: 1.3 Easy

% DCG version

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    sum_list(L,Sum),
    format('~5f~n',Sum).
main.
q([F|Fs]) --> float(F1)," ",float(F2), {F is F1*F2}, eol, q(Fs).
q([]) --> [].
p(F) --> integer(_), eol, q(F).
