% https://open.kattis.com/problems/classfieldtrip
% 1s
% 1.4 Easy

% 123 chars
main :-
    readln(A,end_of_file),
    maplist(string_codes,A,B),
    flatten(B,F),
    msort(F,S),
    format('~s~n',[S]).

/*
% Compressed: 97 chars
main:-readln(A,end_of_file),maplist(string_codes,A,B),flatten(B,F),msort(F,S),format('~s~n',[S]).
*/

/*
% 127 chars
main :-
    rs(A),rs(B),
    append(A,B,AB),
    msort(AB,S),
    format('~s~n',[S]).
rs(S):-read_line_to_codes(user_input,S).
*/