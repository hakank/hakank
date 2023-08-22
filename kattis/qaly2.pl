% https://open.kattis.com/problems/qaly
% Time limit: 1s
% Difficulty: 1.3 Easy

% Using read_string/3.

% 212 chars
main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns,0,Res),
    writeln(Res).
s([],M,M). s([A,B|Ns],M0,M) :- M1 is M0+A*B, s(Ns,M1,M).

/*
% Compressed: 186 chars
main:-read_string(user_input,100000,S),split_string(S,"\n ","\n ",[_|Ss]),maplist(number_string,Ns,Ss),s(Ns,0,Res),writeln(Res).
s([],M,M). s([A,B|Ns],M0,M) :- M1 is M0+A*B, s(Ns,M1,M).

*/

/*
% Another take. 223 chars
main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(s,Ss,Ns),
    sum_list(Ns,Res),
    writeln(Res).
s(S,R) :- split_string(S," ","",T), maplist(number_string,[A,B],T),R is A*B.
*/