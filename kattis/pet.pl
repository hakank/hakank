% https://open.kattis.com/problems/pet
% Time limit: 1s
% Difficulty: 1.4 Easy

main :-
    read_line_to_string(user_input,Line),
    read_four_ints(Line,1,[],Sums),
    sort(2,@>=,Sums,Sort),
    [[Winner,Value]|_] = Sort,
    format('~d ~d~n',[Winner,Value]).
main.


read_four_ints(end_of_file,_Count,Sums,Sums).
read_four_ints(In,Count,Sums0,[[Count,Sum]|Sums]) :-
    split_string(In," ","",Ss),
    maplist(number_string,Ns,Ss),
    sum_list(Ns,Sum),
    read_line_to_string(user_input,In2),
    Count1 is Count + 1,
    read_four_ints(In2,Count1,Sums0,Sums).
