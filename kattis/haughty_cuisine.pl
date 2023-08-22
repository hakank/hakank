% https://open.kattis.com/problems/haughtycuisine
% 1s
% 1.6 Easy

main :-
    readln(_K),
    readln(F),    
    maplist(writeln,F).

/*
% Compressed: 46 chars
main:-readln(_K),readln(F),maplist(writeln,F).
*/

/*
main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",[_|Ss]),
    [F|_] = Ss,
    split_string(F," ","",Fs),
    maplist(writeln,Fs).
main.

*/