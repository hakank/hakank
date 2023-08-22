% https://open.kattis.com/problems/savingdaylight
% 1s
% 2.0 Easy

main :-
    read_string(user_input,1000000000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s([S|Ss]) :-
    split_string(S," ","",[Month,Day,Year,Start,End]),
    split_string(Start,":","",[SHS,SMS]),
    split_string(End,":","",[EHS,EMS]),
    maplist(number_string,[SH,SM,EH,EM],[SHS,SMS,EHS,EMS]),
    T is SH*60 + SM,E is EH*60 + EM,D is E - T,DH is D div 60,DM is D-DH*60,
    format("~s ~s ~s ~d hours ~d minutes~n",[Month,Day,Year,DH,DM]),
    s(Ss).