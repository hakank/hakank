% https://open.kattis.com/problems/knotknowledge
% 1s
% 1.4 Easy

main :-
    read_string(_N),
    split_to_num(ToLearn),
    split_to_num(Known),
    member(Knot,ToLearn),
    not(memberchk(Knot,Known)),
    writeln(Knot).
main.

split_to_num(Ss) :-
    read_string(S),
    split_string(S," ", "", Ss).

read_string(S) :-
    read_line_to_string(user_input,S).

