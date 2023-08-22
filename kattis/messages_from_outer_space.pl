% https://open.kattis.com/problems/messages
% 1s
% 4.8 Medium

% Number of _non-overlapping_ words that can be found.
% I skip this for now...
% TODO

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p([WL,WS]),user_input),
    writeln("word_list:"),
    maplist(f,WL),
    writeln("words:"),
    maplist(f,WS),
    nl,
    s(WS,WL),
    
    nl.
main.

s([W|Ws],WL) :-
    format('checking ~s~n',[W]),
    c(WL,W,0,C),
    writeln(c=C),
    s(Ws,WL).


c([],_,C,C).
c([W|WL],Word,C0,C) :-
    f(W),
    % This is _not_ correct since it also found
    % overlapping words.
    (append([_,W,_],Word) ->
        format('found ~s~n',[W]),
        C1 is C0 + 1
    ;
        C1 is C0
    ),
    c(WL,Word,C1,C).

f(S) :- format('~s~n',[S]).


wl([S|Ss]) --> string_without("\n#|",S), {S \= ""}, eol, wl(Ss).
wl([S]) --> string_without("\n#|",S).
wl([]) --> [].

w1(S) --> string_without("\n#|",S), {S \= ""}, [0'|].

w([S|Ss]) --> w1(S), eol, w(Ss).
w([S]) --> w1(S).
w([]) --> [].

p([WL,WS]) --> wl(WL),{WL \= []}, eol,
               [0'#], eol,
               w(WS), {WS \= []}, eol,[0'#],eol.
p([]) --> [].
    