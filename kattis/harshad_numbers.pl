% https://open.kattis.com/problems/harshadnumbers
% 1s
% 1.5 Easy


main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    h(N,H),
    writeln(H).
main.

h(N,H) :-
    (hn(N) -> H=N ; N1 is N+1, h(N1,H)).

hn(N) :-
    digits_sum(N,S),
    N mod S =:= 0.

digits_sum(N,Sum) :-
        number_chars(N,Chars),
        maplist(atom_number,Chars,Digits),
        sum_list(Digits,Sum).
 