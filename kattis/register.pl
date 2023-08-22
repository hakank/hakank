% https://open.kattis.com/problems/register
% 1s
% 2.1 Easy

% Nope. I miss something here....
% I thing it's the carrying/overflow step that I miss...

% 2,3,5,7,11,13,17,19

main :-
    readln(S,end_of_file),
    writeln(S),
    p(P),
    writeln(p=P),
    s(P,S,[],R),
    writeln(R),
    r(R,1,Res),
    writeln(res=Res).
r([],R,R).
r([H|T],R0,R) :-
    (H>0->R1 is R0*H;R1 is R0),
    r(T,R1,R).
s([],[],R,R).
s([P|Ps],[S|Ss],R0,[D|R]) :-
    D is P-S-1,
    % R1 is R0 + D,
    writeln([p=P,s=S,r0=R0,d=D,r1=R1]),
    s(Ps,Ss,R0,R).
p([2,3,5,7,11,13,17,19]).