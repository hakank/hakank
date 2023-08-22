% https://open.kattis.com/problems/recipes
% 4s
% 1.8 Easy

% P: number of portions for which the recipe was written
% D: number of desired portions

% For the first example:
% - Identify the one with 100%:
%     beef 453.6 100.0
% - P=4 D=20 -> DP = 20/4 = 5
% - multiply 5 with beef's weight: 5*453.6=2268
% - for the other foods: multiply 2268 with the food's percentace
%   e.g. for oliveoil 2268*11.2/100 = 254.016
%     

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(1,Ss).

s(_,[]).
s(I,[S|Ss]) :-
    format("Recipe # ~d~n",[I]),
    split_string(S," ","",T),
    maplist(number_string,[R,P,D],T),
    DP is D/P,
    length(A,R),
    append(A,Rs,Ss),
    g(A,Ls),
    member([_,W,100.0],Ls),
    C is W*DP,
    p(Ls,C),
    
    e(E),    
    writeln(E),
    I1 is I+1,
    s(I1,Rs).

p([],_).
p([[N,_,Pct]|Ss],C) :-
    W is Pct*C/100,
    format("~s ~1f~n",[N,W]),
    p(Ss,C).

g([],[]).
g([S|Ss],[[Name,Wt,Pct]|L]) :-
    split_string(S," ","",[Name|T]),
    maplist(number_string,[Wt,Pct],T),
    g(Ss,L).

e(----------------------------------------).