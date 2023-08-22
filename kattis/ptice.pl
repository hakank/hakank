% https://open.kattis.com/problems/ptice
% 1s
% 1.6 Easy

% Adrian: A,B,C,A,B,C,...


% Bruno: B,A,B,C,B,A,B,C

% Goran: C,C,A,A,B,B

main :-
    read_string(user_input,1000,S),
    split_string(S,"\n","",[_,T,""]),
    string_chars(T,Ls),
    length(Ls,Len),
    Ns=["Adrian","Bruno","Goran"],
    A=['A','B','C'],B=['B','A','B','C'],G=['C','C','A','A','B','B'],Ps = [A,B,G],
    findall(M,(member(P,Ps),s(0,Len,Ls,P,0,M)),Ms),
    max_list(Ms,Max),
    writeln(Max),
    findall(_,(member(I,[1,2,3]),
               (
                   nth1(I,Ms,Max) ->
                       nth1(I,Ns,P),writeln(P)
                   ;
                       true)
               ),_).
main.

s(N,N,[],_,T,T).
s(I,N,[C|Cs],Ps,T0,T) :-
    length(Ps,PsLen),
    J is 1+(I mod PsLen),
    nth1(J,Ps,P),
    (C == P ->
        T1 is T0+1
    ;
        T1 is T0
    ),
    I1 is I+1,
    s(I1,N,Cs,Ps,T1,T).
