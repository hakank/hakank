% https://open.kattis.com/problems/ofugsnuid
% 1s
% 1.3-1.5 Easy

% Uncompressed: 80 chars
main :-
    readln([_|S],end_of_file),
    reverse(S,R),
    maplist(writeln,R).



/*
% Shorter using readln/2: 64 chars. But it's much slower than reverse4.pl: 0.74s vs 0.24s.
main:-readln([_|S],end_of_file),reverse(S,R),maplist(writeln,R).
*/
