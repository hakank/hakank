% https://open.kattis.com/problems/fodelsedagsmemorisering
% 1s
% 1.2 - 1.7 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss,[],SRes),
    sort(SRes,Rs),
    findall(DM,member([DM,_,_],Rs),DMs),
    sort(DMs,DMSorted),
    findall(Name,(member(DM,DMSorted),
                  findall([Like,Name],member([DM,Like,Name],Rs),DMS),
                  sort(1,@>,DMS,Sorted),
                  [[_,Name]|_]=Sorted
                 ),
            Res),
    sort(Res,ResSorted),
    length(ResSorted,Len),
    writeln(Len),
    maplist(writeln,ResSorted).
main.

s([],S,S).
s([L|Ls],S0,[[DM,Like,Name]|S]) :-
    split_string(L," ","",[Name,LikeS,DM]), 
    number_string(Like,LikeS),
    s(Ls,S0,S).
