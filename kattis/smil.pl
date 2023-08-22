% https://open.kattis.com/problems/smil
% CPU Time limit: 1s
% Difficulty: 1.3-1.5 Easy
%
:- [kattio].

% sub_string(word, N, Len, M, substring)
% where you pass in word and substring, prolog will match N, Len and M. For example
% sub_string(+String, ?Before, ?Length, ?After, ?SubString)
main :-
    read_atom(String),
    % format('~s~n',[String]),
    member(Ss,[':)',';)',':-)',';-)']),
    sub_string(String, Before, _Length, _After, Ss),
    writeln(Before),
    fail,
    nl.
main.
