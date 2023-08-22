% https://open.kattis.com/problems/heirsdilemma
% 1s
% 1.7s

% Time Limit Exeeded on 6/8.
% Ok, trying some heuristics.
% The fastest seems to be enum,down (1.03s on my machine and still too slow).
% Picat is a bit faster: 0.05s
% OK, back to the drawing board: heirs_dilemma2.pl

:- use_module(library(clpfd)).
main :-
    read_string(user_input,100000,S),
    split_string(S," ","\n",Ss),
    maplist(number_string,[Low,Up],Ss),
    findall(N,s(Low,Up,N),Sol),
    length(Sol,Len),
    writeln(Len).

s(Low,Up,N) :-
    length(X,6),
    X ins 1..9,
    N in Low..Up,
    all_different(X),    
    to_num(X,1,6,10,0,N),
    append(X,[N],Vars),
    d(X,N),
    labeling([enum,down],Vars).
    

d([],_).
d([D|Ds],N) :-
    N mod D #= 0,
    d(Ds,N).
    

to_num([],_I,_Len,_Base,Num,Num).
to_num([H|T],I,Len,Base,Num0,Num) :-
        Len1 #= Len-I,
        Num1 #= Num0 + H*(Base^Len1),
        I1 #= I+1,
        to_num(T,I1,Len,Base,Num1,Num).

