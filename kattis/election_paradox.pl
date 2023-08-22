% https://open.kattis.com/problems/electionparadox
% 1s
% 1.9 Easy

% Assumptions:
% A gets the most individual votes
% B get the more majority states than A
%
% Algorithm:
%  - sort the list
%  - for the first Len/2 states A get everything (and B get 0)
%  - for the rest of the states B gets N div 2 + 1 votes and A gets the rest
%  This results in that A got a total of votes larger than B, but B gets
%  more majorities votes. This is the paradox.

main :-
    read_string(user_input, 1000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    sort(0,@>=,Ns,Nss),
    length(Nss,Len),
    Len2 is Len // 2,    
    length(A,Len),
    length(B,Len),
    s(0,Len2,Nss,A,B),
    sum_list(A,ASum),
    writeln(ASum).
main.

s(_,_,[],[],[]).
s(I,Limit,[N|Ns],[A|As],[B|Bs]) :-
    (I < Limit ->
        A is N,
        B is 0
    ;
        Div is N div 2,
        B is Div+1,
        A is N - B
    ),
    I1 is I+1,
    s(I1,Limit,Ns,As,Bs).
