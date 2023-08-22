% https://open.kattis.com/problems/volim
% 1s
% 1.7 Easy

% Forgot that it's a circle -> a special mod to handle (8+1)->1 instead of (8+1)->9
% Note: The proper "special mod" is
%   1+((X-1) mod 8)
% But X is here N+1, hence
% ->
%   1+(N mod 8)

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[K0,_|Ss]),
    number_string(K,K0),
    s(Ss,K,0).

s([],_,_).
s([S|Ss],N,B0) :-
    split_string(S," ","",[TS,A]),
    number_string(T,TS),
    B1 is B0+T,
    (B1 >= 210 -> writeln(N), true
    ;
        ( A == "T"  -> N1 is 1+(N mod 8); N1 is N ),
        s(Ss,N1,B1)
    ).

% mod2(N,Mod,Res) :- M1 is N mod Mod, (M1 =:= 0 -> Res = Mod ; Res = M1).