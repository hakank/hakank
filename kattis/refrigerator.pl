% https://open.kattis.com/problems/refrigerator
% Time limit: 1s
% Difficulty 1.6-2.4 Easy

% First: 33/100: 33/33, wrong answer on the next group.
% Fixed, it should be #>= NumRefrigerators, not #=
% 
:- use_module(library(clpfd)).
main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ns0),
    maplist(number_string,Ns,Ns0),
    [CostTripA,CapacityTripA,CostTripB,CapacityTripB,NumRefrigerators] = Ns,
    solve(CostTripA,CapacityTripA,CostTripB,CapacityTripB,NumRefrigerators, NumA,NumB, TotalCost),
    format('~d ~d ~d~n',[NumA,NumB,TotalCost]).
main.

solve(CostTripA,CapacityTripA,CostTripB,CapacityTripB,NumRefrigerators, NumA,NumB, TotalCost) :-
    NumA in 0..NumRefrigerators,
    NumB in 0..NumRefrigerators,
    NumA*CapacityTripA + NumB*CapacityTripB #>= NumRefrigerators,
    TotalCost #= CostTripA*NumA + CostTripB*NumB,
    labeling([min(TotalCost)],[NumA,NumB]).
    
    