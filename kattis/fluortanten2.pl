% https://open.kattis.com/problems/fluortanten
% Time limit: 3s
% Difficulty: 1.2-2.4 Easy

% Not totally correct:
% Group 1 was correct but not group 2 and 3 (Time Limit Exceeded)

% Skipping clpfd. Using read_string/3.
% There must be a simpler algo for this,
% which involves a moving sum... 

main :-
    read_string(user_input,1000000000,S),
    split_string(S,"\n ", "\n ", [_|Ns0]),
    maplist(number_string,Ns,Ns0),
    length(Ns,Len),
    numlist(1,Len,Is),
    select(0,Ns,Ns2),
    maplist(happiness(Ns2,Is),Is,Happiness),
    max_list(Happiness,Max),
    writeln(Max).
main.

happiness(Ns2,Is,I,Happiness) :-
    I0 is I-1,
    nth0(I0,Ns3,0,Ns2),
    % isum(Ns3,1,0,Happiness).
    scalar_product2(Ns3,Is,Happiness).

isum([],_,S,S).
isum([N|Ns],I,S0,S) :-
    % writeln(isum([n=N|Ns],i=I,s0=S0,S)),
    S1 is S0+N*I,
    I1 is I+1,
    isum(Ns,I1,S1,S).


scalar_product2(L1,L2,Res) :-
    scalar_product2(L1,L2,0,Res).
scalar_product2([],[],Res,Res).
scalar_product2([X|Xs],[Y|Ys],Res0,Res) :-
    Res1 is Res0 + X*Y,
    scalar_product2(Xs,Ys,Res1,Res).
