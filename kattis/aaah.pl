% https://open.kattis.com/problems/aaah
% 1s
% 1.9 Easy

% 94 chars
main :-
    readln([A,B],end_of_file),
    (sub_string(A,_,_,_,B)->T=go;T=no),
    writeln(T).

/*
% Compressed: 78 chars Not short enough for Top 10 (25..32 chars)
main:-readln([A,B],end_of_file),(sub_string(A,_,_,_,B)->T=go;T=no),writeln(T).

*/

/*
main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[A,B]),
    (sub_string(A,_,_,_,B)->T=go;T=no),
    writeln(T).
*/