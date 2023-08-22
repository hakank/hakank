% https://open.kattis.com/problems/kleptography
% 1s
% 1.5 Easy

% c(S,Len,Res)
% Res is the string S copied to be of the exact length Len

% TODO, I don't get the encryption scheme. It's not again+again+again,
% rather againplaintext...

main :-
    rs(S1),
    split_string(S1," ","",S1s),
    maplist(number_string,[L1,L2],S1s),
    writeln([l1=L1,l2=L2]),
    rs(S2),
    writeln(S2),
    rs(S3),
    writeln(s3=S3),
    length(S3,S3Len),
    % A="abcdefghijklmnopqrstuvwxyz",
    c(S2,S3Len,[],S2C),
    writeln(s2_copy=S2C),
    format('copy:~s~n',[S2C]),
    d(S2C,S3,[],E),
    writeln(s2=S2C),
    writeln(s3=S3),
    writeln('e '=E),
    format('E:~s~n',[E]),    
    nl.
main.

d([],[],R,R).
d([P|Ps],[C|Cs],R0,[E|R]) :-
    d2(P,C,E),
    d(Ps,Cs,R0,R).

d2(P,C,E) :-
    nl,
    %  A="abcdefghijklmnopqrstuvwxyz",
    A=[97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122],
    nth0(P1,A,P),
    nth0(P2,A,C),
    X is (P2+P1) mod 26,
    nth0(X,A,E),
    format('p=~c p1=~d c=~c p2=~d x=~d e=~c~n',[P,P1,C,P2,X,E]).
    
    

c(_S,Len,R,R) :-
    length(R,Len).
c(S,Len,R0,R) :-
    length(S,SLen),
    length(R0,R0Len),
    Diff is Len - R0Len,
    (Diff >= SLen ->
        append(S,R0,R2)
    ;
        min_list([Diff,SLen],MinLen),
        length(S2,MinLen),
        append(S2,_,S),
        append(S,S2,R2)
    ),
    c(S,Len,R2,R).
        
    


rs(S) :- read_line_to_codes(user_input,S).