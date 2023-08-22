% https://open.kattis.com/problems/howl
% 1s
% 1.9 Easy

/*
* It must consist of a combination of the letters A, H, O and W. 
  Each letter must occur at least once.

* The howl can not contain two consecutive W’s, or two consecutive H’s.

* The howl can not contain an H followed immediately by a W or an A.

* There can never be an A after the first occurrence of an O.

*/

% 93 chars
main :-
    read_line_to_string(user_input,S),
    string_concat(S,"O",SA),
    writeln(SA).

/*
% Compressed: 76 chars
main:-read_line_to_string(user_input,S),string_concat(S,"O",SA),writeln(SA).

*/

% And here is a more principled solution which generates
% valid howls using a DCG.
main2 :-
    read_line_to_string(user_input,S),
    string_length(S,Len),
    writeln(Len),
    Len1 is Len+1,
    length(Howl,Len1),    
    phrase(s,Howl),
    memberchk(0'A,Howl),
    memberchk(0'H,Howl),
    memberchk(0'O,Howl),
    memberchk(0'W,Howl),
    not(append([_,[0'A,0'A],_],Howl)),
    not(append([_,[0'H,0'W],_],Howl)),
    not(append([_,[0'H,0'A],_],Howl)),
    not(append([_,[0'O],_,[0'A],_],Howl)),            
    format("~s~n",[Howl]),
    fail,
    nl.

v([0'A,0'H,0'O,0'W]).

s --> [C], {v(Cs),member(C,Cs)},s.
s --> [].
