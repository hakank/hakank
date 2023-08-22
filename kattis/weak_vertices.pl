% https://open.kattis.com/problems/weakvertices
% 1s
% 1.6 Easy

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).

s([]).
s(["-1"]).
s([S|Ss]) :-
    number_string(N,S),
    length(MatS,N),
    append(MatS,Rest,Ss),
    maplist(sp,MatS,Mat),
    length(Mat,Size),
    Size1 is Size -1,
    adj(Mat,Size,0,[],AdjL),
    flatten(AdjL,L),
    once(findall(V,(between(0,Size1,V),not(c(V,L))),Vs)),
    maplist(format("~d "),Vs),
    nl,
    s(Rest).

sp(L,M) :- split_string(L," ","",MS), maplist(number_string,M,MS).

% Convert to adjacency list
adj([],_,_,L,L).
adj([H|T],N,I,L0,[Adj|L]) :-
    findall([I-J,J-I],(between(1,N,J),nth0(J,H,1)),Adj),
    I1 is I+1,
    adj(T,N,I1,L0,L).
    

c(I,Adj) :-
    member(I-J,Adj),member(J-K,Adj),member(K-I,Adj).
