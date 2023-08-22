% https://open.kattis.com/problems/nineknights
% 1s
% 2.1 Easy


% readln/2 gave wrong answer on test 4/23.
% fixed with read_string/3.

main :-
    read_string(user_input,_,L),
    split_string(L,"\n ","\n ",Ls),
    maplist(string_chars,Ls,Ss),
    flatten(Ss,S),
    s(S,0,[],T0),
    sort(T0,T),
    length(T,Len),
    ((Len=:= 9,c(T)) -> writeln("valid");writeln("invalid")).

c([]).
c([R-C|Ks]) :-
    n(R,C,Ns),
    forall(member(N,Ns),not(memberchk(N,Ks))),
    c(Ks).
n(R,C,Ns) :-
    V = [-2,-1,1,2],
    findall(R1-C1,(member(A,V),member(B,V),
                   abs(A)+abs(B) =:= 3,
                   R1 is R+A, C1 is C+B,
                   R1 >= 0, R1 =< 4, C1 >= 0, C1 =< 4
                  ),
            Ns).
s([],_,T,T).
s([K|Ks],I,T0,T) :-
    (K == 'k' ->
        p(I,R,C),
        append(T0,[R-C],T1)
    ;
        T1 = T0
    ),
    I1 is I+1,
    s(Ks,I1,T1,T).
p(A,R,C):-R is A div 5,C is A mod 5.


/*
% Compressed: 554 chars, not short enough for Top 10 (283..405 chars)
main:-read_string(user_input,_,L),split_string(L,"\n ","\n ",Ls),maplist(string_chars,Ls,Ss),
flatten(Ss,S),s(S,0,[],T0),sort(T0,T),length(T,Len),((Len=:= 9,c(T))->writeln("valid");writeln("invalid")).
c([]). c([R-C|Ks]):-n(R,C,Ns),forall(member(N,Ns),not(memberchk(N,Ks))),c(Ks).
n(R,C,Ns):-V=[-2,-1,1,2],findall(R1-C1,(member(A,V),member(B,V),abs(A)+abs(B) =:= 3,
R1 is R+A,C1 is C+B,R1>=0,R1=<4,C1>=0,C1=<4), Ns).
s([],_,T,T). s([K|Ks],I,T0,T):-(K=='k'->p(I,R,C),append(T0,[R-C],T1);T1=T0),I1 is I+1,s(Ks,I1,T1,T).
p(A,R,C):-R is A div 5,C is A mod 5.
*/