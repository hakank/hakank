% https://open.kattis.com/problems/magictrick
% 1s
% 1.4 Easy

main :-
    read_line_to_codes(user_input,C),
    length(C,CL),
    sort(C,S),
    (length(S,CL)->T=1;T=0),
    writeln(T).

/*
% Compressed: 97 chars
main:-read_line_to_codes(user_input,C),length(C,CL),sort(C,S),(length(S,CL)->T=1;T=0),writeln(T).

*/

/*
main :-
    read_line_to_codes(user_input,C),
    length(C,CL),
    sort(C,S),
    length(S,SL),
    (CL =:= SL -> writeln(1) ; writeln(0)).
*/

/*
% Compressed: 103 chars
main:-read_line_to_codes(user_input,C),length(C,CL),sort(C,S),length(S,SL),(CL=:=SL->T=1;T=0),write(T).
*/
