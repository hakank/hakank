% https://open.kattis.com/problems/exam
% 1s
% 1.8 Easy

% Wrong answer on 3/10
% Without the second (empty) main/0 it gave a Run Time Error
% so it's probably some edge case that I haven't thought of.
% Another difference is exam3.inp (0,FFFFF,FFFFF) which exam_cp.pl give 5
% but it should be 0 (which exam.pl yields).
%
% I leave this since exam.pl (non CP) works.%
%
:- use_module(library(clpfd)).
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[NS,MeS,FriendS]),
    number_string(N,NS),
    maplist(string_codes,[MeS,FriendS],[MeC,FriendC]),
    maplist(c,MeC,Me),
    writeln('me    '=Me),
    maplist(c,FriendC,Friend),
    writeln(friend=Friend),
    length(Me,Len),    
    length(X,Len),
    X ins 0..1,
    scalar_product(Friend,X,#=,N),
    s(X,Me,0,T),
    labeling([max(T)],X),
    writeln('x     '=X),
    writeln(T).
main.

s([],[],S,S).
s([X|Xs],[M|Ms],S0,S) :-
    (X#=M) #==> S1 #= S0 + 1,
    (X#\=M) #==> S1 #= S0,
    s(Xs,Ms,S1,S).

c(0'F,0).
c(0'T,1).