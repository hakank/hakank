% https://open.kattis.com/problems/oktalni
% 1s
% 1.9 Easy

%% Picat> S="011001100",[parse_radix_string(C,2).to_radix_string(8) : C in chunks_of(S,3),println(c=C)]=X
%% c = 011
%% c = 001
%% c = 100
%% S = ['0','1','1','0','0','1','1','0','0']
%% X = [['3'],['1'],['4']]
%% yes

main :-
    read_line_to_codes(user_input,Cs),
    length(Cs,Len),
    (Len mod 3=:=0->L=Cs;c(3-(Len mod 3),0'0,Cs,L)),
    s(L), nl.
s([]).
s([A,B,C|Ss]) :-
    maplist(t,[A,B,C],P),
    b(P,X),write(X),s(Ss).
b([0,0,0],0).
b([0,0,1],1).
b([0,1,0],2).
b([0,1,1],3).
b([1,0,0],4).
b([1,0,1],5).
b([1,1,0],6).
b([1,1,1],7).
t(N,B) :- B is N-48.
c(0,_,L,L).
c(I,C,L0,[C|L]) :-
    I1 is I-1,
    c(I1,C,L0,L).
