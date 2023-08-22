% https://open.kattis.com/problems/quickbrownfox
% 1s
% 1.8 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).

s([]).
s([S|Ss]) :-
    string_lower(S,L),
    string_codes(L,Codes),
    findall(C,(between(0'a,0'z,C),not(memberchk(C,Codes))),Cs),
    (Cs == [] ->
        writeln("pangram")
    ;
        sort(Cs,Sorted),
        format('missing ~s~n',[Sorted])
    ),
    s(Ss).
    

