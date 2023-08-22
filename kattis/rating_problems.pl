% https://open.kattis.com/problems/ratingproblems
% 1s
% 1.4 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p([NumJudges,NumVoted,L]),user_input),
    sum_list(L,Sum),
    NotVoted is NumJudges - NumVoted,
    Low is (Sum + NotVoted * -3)/NumJudges,
    High is (Sum + NotVoted * 3)/NumJudges,    
    format('~6f ~6f~n',[Low,High]).
main.

q1(N) --> integer(N).
q([N|Ns]) --> q1(N), eol, q(Ns).
q([N]) --> q1(N).
q([]) --> [].

p([NumJudges,NumVoted,L]) --> integer(NumJudges), " ", integer(NumVoted), eol, q(L).
