% https://open.kattis.com/problems/fluortanten
% Time limit: 3s
% Difficulty: 1.2-2.4 Easy

% Not totally correct:
% Group 1 was correct but not group 2 and 3 (Time Limit Exceeded)

:- use_module(library(clpfd)).
main :-
    read_line_to_string(user_input,_),
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ns0),
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
    scalar_product(Ns3,Is,#=,Happiness),
    labeling([ff,bisect],Ns3). % Adding labeling didn't help...
