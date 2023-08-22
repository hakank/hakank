% https://open.kattis.com/problems/alphabetspam
% Time limit: 1s
% Difficulty: 1.4 Easy

main :-
    read_line_to_codes(user_input,Codes),
    length(Codes,Len),
    cs(Codes,0,Ws,0,Ls,0,Us,0,Ss),
    WsF is Ws / Len,
    LsF is Ls / Len,
    UsF is Us / Len,
    SsF is Ss / Len,
    maplist(format('~16f~n'),[WsF,LsF,UsF,SsF]).
main.

% whitespace characters, lowercase letters, uppercase letters, and symbols
cs([],Ws,Ws,Ls,Ls,Us,Us,Ss,Ss).
cs([C|Cs],Ws0,Ws,Ls0,Ls,Us0,Us,Ss0,Ss) :-
    cs(C,T),
    (T == whitespace ->  Ws1 is Ws0 + 1  ; Ws1 is Ws0 ),
    (T == lower ->  Ls1 is Ls0 + 1  ; Ls1 is Ls0 ),
    (T == symbol ->  Ss1 is Ss0 + 1  ; Ss1 is Ss0 ),
    (T == upper ->  Us1 is Us0 + 1  ; Us1 is Us0 ),            
    cs(Cs,Ws1,Ws,Ls1,Ls,Us1,Us,Ss1,Ss).

% 33-64: symbols
% 65-90: upper
% 91-96: symbols
% 97-122: lower
% 123-126: symbols

cs(95, whitespace) :- !.
cs(C, symbol)  :- C >=  33, C =<  64, !.
cs(C, upper)   :- C >=  65, C =<  90, !.
cs(C, symbol)  :- C >=  91, C =<  96, !.
cs(C, lower)   :- C >=  97, C =< 122, !.
cs(C, symbol)  :- C >= 123, C =< 126, !.
