% https://open.kattis.com/problems/datum
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[D,M],Ss),
    day_of_the_week(date(2009,M,D),X),
    nth1(X,["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"],Y),
    writeln(Y).
main.
