% https://open.kattis.com/problems/grassseed
% 1s
% 1.4 Easy

main :-
    ri(Cost),
    ri(_),
    ra(Area),
    Total is Cost * Area,
    format('~10f~n',[Total]).
main.

ra(Total) :-
    rs(S),
    ra(S,0,Total).

ra(end_of_file,L,L) :- !.
ra(S,L0,L) :-
    split_string(S," ", "", Ss),
    maplist(number_chars,[A,B],Ss),
    L1 is L0 + A*B,
    rs(S2),    
    ra(S2,L1,L).

rs(S) :-
    read_line_to_string(user_input,S).

ri(N) :-
    rs(S),
    number_string(N,S).
