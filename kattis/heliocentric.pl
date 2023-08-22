% https://open.kattis.com/problems/heliocentric
% 1s
% 1.7 Easy

% Earth: 365 days orbit
% Mars:  687 days

% This takes 0.06s.
% There is probably a faster/smarter way of solving this.
% heliocentric2.pl is shorter (382 chars vs 412 chars),
% but it's slower 0.26s (vs 0.06s for this program).

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",Ss),
    s(1,Ss).
main.

s(_,[]).
s(I,[L|Ls]) :-
    split_string(L," ","",Ss),
    maplist(number_string,[E,M],Ss),
    t(0,E,M,T),
    format('Case ~d: ~d~n',[I,T]),
    I1 is I+1,
    s(I1,Ls).

t(I,E,M,Time) :-
    ( ((E+I) mod 365 =:= 0, (M+I) mod 687 =:= 0) ->
        Time = I
    ;
        I1 is I+1,
        t(I1,E,M,Time)
    ).