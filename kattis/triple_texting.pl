% https://open.kattis.com/problems/tripletexting
% 2s
% 1.7 Easy

main :-
    read_line_to_codes(user_input,S),
    length(S,Len),
    Len2 is Len div 3,
    length(W1,Len2),
    length(W2,Len2),
    a(W1,W2,W3,S),
    msort([W1,W2,W3],Ws),
    clumped(Ws,Cl),
    sort(2,@>,Cl,Ss),
    ([T-3|_]=Ss;[T-2|_]=Ss),
    format('~s~n',[T]).
main.
a(A,B,C):-append(A,B,C). a(X,Y,Z,L):-a(X,R,L),a(Y,Z,R).