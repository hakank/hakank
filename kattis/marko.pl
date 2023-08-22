% https://open.kattis.com/problems/marko
% 1s
% 1.9 Easy

main :-
    read_string(user_input, 10000000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    append(WsS,[Seq],Ss),
    string_chars(Seq,Cs),
    maplist(string_chars,WsS,Ws),   
    findall(X,(member(C,Cs),t(X,C)),Xs),
    s(Ws,Xs,0,Count),
    writeln(Count).

s([],_,C,C).
s([W|Ws],Xs,C0,C) :-
    (c(W,Xs)->C1 is C0 +1;C1 is C0),
    s(Ws,Xs,C1,C).
c([],_).
c([C|Cs],[X|Xs]) :-
    (memberchk(C,X)->c(Cs,Xs);false).
t([a,b,c],'2').
t([d,e,f],'3').
t([g,h,i],'4').
t([j,k,l],'5').
t([m,n,o],'6').
t([p,q,r,s],'7').
t([t,u,v],'8').
t([w,x,y,z],'9').
t([' '],'0').
