% https://open.kattis.com/problems/socialrunning
% 1s
% 2.2 Easy

% The Python3 program social_running.py got place 9 in Top 10

% 196 chars
main :-
    readln([_|L],end_of_file),
    append(L,L,L2),
    s(L2,D),
    min_list(D,M),
    writeln(M).
s([],[]).
s([_],[]).
s([_,_],[]).
s([A,B,C|Ds],[X|D]) :-
    X is A+C,
    s([B,C|Ds],D).

/*
% Compressed: 159 chars. Not short enough for Top 10 (47..119)
main:-readln([_|L],end_of_file),append(L,L,L2),s(L2,D),min_list(D,M),writeln(M).
s([],[]). s([_],[]). s([_,_],[]). s([A,B,C|Ds],[X|D]):-X is A+C,s([B,C|Ds],D).
*/

/*
% 219 chars
main :-
    readln([_|L],end_of_file),
    length(L,Len),
    Len2 is Len+1,
    append(L,L,L2),
    findall(D,(between(1,Len2,I),nth1(I,L2,V),I2 is I+2,nth1(I2,L2,V2),D is V+V2),Ds),
    min_list(Ds,M),
    writeln(M).
*/