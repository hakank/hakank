% https://open.kattis.com/problems/fastfood
% 1s
% 2.1 Easy

% TODO: This is messy now.
% It should really be a while loop to see how many instances of the prizes
% that can be collected

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([NS|Ss]) :-
    nl,
    split_string(NS," ","",[N0,S0]),
    maplist(number_string,[N,S],[N0,S0]),
    writeln([n=N,s=S]),
    N1 is N+1,
    length(A,N1),
    append(A,Rest,Ss),
    maplist(p,A,As),    
    writeln(as=As),
    append(Ls,[Ps],As),
    writeln([ls=Ls,ps=Ps]),        
    i(Ps,1,d{},DB),
    writeln(db=DB),
    c(Ls,DB,0,R),
    writeln(R),
    
    s(Rest).
p(L,Ls) :- split_string(L," ","",Ls0),maplist(number_string,Ls,Ls0).
i([],_,D,D).
i([N|Ns],I,D0,D) :-
    writeln([n=N,i=I]),
    D1=D0.put(I,N),
    writeln(d1=D1),
    I1 is I+1,
    i(Ns,I1,D1,D).
    
c([],_,R,R).
c([L|Ls],DB,R0,R) :-
    append([_|A],[W],L),
    writeln([a=A,w=W]),
    (c1(A,W,DB,DB1) ->
        R1 is R0+W
    ;
        R1 is R0
    ),
    writeln(r1=R1),
    c(Ls,DB1,R1,R).

c1([],_,DB,DB).
c1([N|Ns],W,DB0,DB) :-
    writeln([n=N,db0=DB0]),
    ( (V=DB0.get(N),V>0) ->
        V1 is V-1,
        DB1=DB0.put(N,V1)
    ;
        fail
    ),
    c1(Ns,W,DB1,DB).
    
    