% https://open.kattis.com/problems/fallingapart
% 1s
% 1.7 Easy

% Shorter: 307 chars

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    sort(0,@>=,Ns,NsS),
    length(Ns,N),
    p(NsS,N,1,A),p(NsS,N,0,B),
    format('~d ~d~n',[A,B]).
p(L,N,M,S):-findall(V,(between(1,N,I),I mod 2=:=M,nth1(I,L,V)),Ss),sum_list(Ss,S).

/*

% Compressed: 271 chars
main:-read_string(user_input,10000000,S),split_string(S,"\n ","\n ",[_|Ss]),maplist(number_string,Ns,Ss),sort(0,@>=,Ns,NsS),length(Ns,N),p(NsS,N,1,A),p(NsS,N,0,B),format('~d ~d~n',[A,B]).
p(L,N,M,S):-findall(V,(between(1,N,I),I mod 2=:=M,nth1(I,L,V)),Ss),sum_list(Ss,S).


*/