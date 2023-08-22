% https://open.kattis.com/problems/everywhere
% 1s
% 1.4 Easy

main :-
    read_codes(_),
    repeat,
    read_codes(S),
    (S == end_of_file ;
        number_chars(Num,S),
        read_trip(Num,0,S,[],[_|Trip]),
        sort(Trip,Distinct),
        length(Distinct,Len),
        writeln(Len),
        fail
    ).

read_trip(Num,Num,end_of_fie,Trip,Trip) :- !.
read_trip(Num,Num,S,Trip,[S|Trip]) :- !.
read_trip(Num,C,S,Trip0,[S|Trip]) :-
    C1 is C + 1,
    read_codes(S2),    
    read_trip(Num,C1,S2,Trip0,Trip).

read_codes(S) :-
    read_line_to_codes(user_input,S).
