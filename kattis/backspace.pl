% https://open.kattis.com/problems/backspace
% 1s
% 2.2 Easy

% 265 chars
main :-
    read_line_to_codes(user_input,S),
    s(S,[],T),
    reverse(T,V),
    string_chars(R,V),
    writeln(R).
s([],R,R).
s([C|Cs],R0,R) :-
    (C == 0'< ->
        [_|R1]=R0
    ;
        [C|R0]=R1
    ),
    s(Cs,R1,R).

/*
% compressed: 175 chars. Not short enough for Top 10 (47..90)
main:-read_line_to_codes(user_input,S),s(S,[],T),reverse(T,V),string_chars(R,V),writeln(R).
s([],R,R). s([C|Cs],R0,R):-(C==0'<->[_|R1]=R0;[C|R0]=R1),s(Cs,R1,R).

*/

/*
% 277 chars
main :-
    read_string(user_input,_,S),
    string_chars(S,Ss),
    s(Ss,[],T),
    reverse(T,V),    
    string_chars(R,V),
    writeln(R).
s([],R,R).
s(['\n'|_],R,R).
s([C|Cs],R0,R) :-
    (C == '<' ->
        [_|R1]=R0
    ;
        append([C],R0,R1)
    ),
    s(Cs,R1,R).
*/
/*
% Compressed: 200 chars, not short enough for Top 10 (47..90)
main:-read_string(user_input,_,S),string_chars(S,Ss),s(Ss,[],T),reverse(T,V),string_chars(R,V),writeln(R).
s([],R,R). s(['\n'|_],R,R). s([C|Cs],R0,R):-(C=='<'->[_|R1]=R0;append([C],R0,R1)),s(Cs,R1,R).

*/