% https://open.kattis.com/problems/socialdistancing2
% 1s
% 2.2 Easy

% 176 chars: Top 10 place 6
% Here's the original logic: The two "-1" are reduced to -2 for brevity.
% findall(C,(nextto(A,B,L), D is B-A-1,C is(D-1)div 2),Ds),

% Shorter: 169 chars: Top 10, still place 6
main :-
    readln([N,_,H|T],end_of_file),
    Y is H+N,
    append([H|T],[Y],L),
    findall(D,(nextto(A,B,L),D is(B-A-2)div 2),Ds),
    sum_list(Ds,R),
    writeln(R).

/*
% Compressed: 139 chars: Top 10 place 4 (before my Python3 solution)
main:-readln([N,_,H|T],end_of_file),Y is H+N,append([H|T],[Y],L),
findall(D,(nextto(A,B,L),D is(B-A-2)div 2),Ds),sum_list(Ds,R),writeln(R).
*/