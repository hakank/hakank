% https://open.kattis.com/problems/pauleigon
% 1s
% 1.7 Easy

% Note: The formula for the next server is
%   T = P+Q+1 % the next point to play
%   ((T-1) div N) mod 2, % adjust for div by -1: if 0 -> paul, if 1 -> opponent
% which is
%   ((P+Q+1-1) div N) mod 2,
% -> 
% ((P+Q) div N) mod 2,

main :-
    readln([N,P,Q],end_of_file),
    V is ((P+Q) div N) mod 2,
    nth0(V,[paul,opponent],R),
    writeln(R).

/*
% Compressed: 93 chars
main:-readln([N,P,Q],end_of_file),V is((P+Q)div N)mod 2,nth0(V,[paul,opponent],R),writeln(R).

*/


/*
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[N,P,Q],Ss),
    V is ((P+Q) div N) mod 2,
    nth0(V,[paul,opponent],R),
    writeln(R).
*/