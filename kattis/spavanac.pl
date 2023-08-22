% https://open.kattis.com/problems/spavanac
% 1s
% 1.5 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ss),
    maplist(number_string,[H,M],Ss),
    Mins is H*60+M-45,
    NewH is (Mins div 60) mod 24,
    NewM is (Mins-NewH*60) mod 60,
    format('~d ~d~n',[NewH,NewM]).
main.




