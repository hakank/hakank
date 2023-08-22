% https://open.kattis.com/problems/jewelrybox
% 1s
% 1.7 Easy

% TODO!

/*
               X
    -----------------------------
    |       ____________        |
    |      |      a     | h     |
    | ----- -. . . . . .------  | 
  Y | |    b .         .     |  | 
    | ---- - . . . . . . ----   |
    |     |_____________|       |
    |                           |
    -----------------------------


  We want to maximize the value of
    4*a*h  + b*a
 or minimize the unused four square corners value of
    4*h*h

 But I cannot use clpr since it doesn't support
 minimize/maximize on nonlinear constraints....

*/  

% ?- use_module(library(clpr)).
main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).

s([]).
s([L|Ls]) :-
    writeln(l=L),
    split_string(L," ","",Ns),
    writeln(ns=Ns),
    maplist(number_string,[X,Y],Ns),
    writeln([x=X,y=Y]),
    %% {A>0.0,B>0.0,H>0.0, H+B+H=<Y,H+A+H=<X
    %%  , T =:= 4*A*B+B*A
    %% },
    writeln([a=A,b=B,h=H,t=T]),
    % T = (A+A+A+A)*H+B*A,
    writeln(after=T),
    maximize(T),
    writeln(t=T),
    s(Ls).