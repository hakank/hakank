% https://open.kattis.com/problems/easiest
% 1s
% 1.6 Easy

main :-
    cl(Ns),
    p(Ns).
main.    

p([]).
p([0]).
p([N|Ns]) :-
    digits_sum(N,Sum),
    c(11,N,Sum,P),
    writeln(P),
    p(Ns).

c(I,N,Sum,P) :-
    NI is I*N,
    (digits_sum(NI,Sum) ->
        P = I
    ;
        I1 is I+1,
        c(I1,N,Sum,P)
    ).

digits_sum(N,Sum) :-
        number_chars(N,Chars),
        maplist(atom_number,Chars,Digits),
        sum_list(Digits,Sum).

cl(Lines) :-
    rs(X),
    cl(X,[],Lines).
cl(X,Lines,Lines) :-
    X == end_of_file, !.
cl(X,Lines0,[N|Lines]) :-
    number_string(N,X),
    rs(X2),
    cl(X2,Lines0,Lines).
rs(S) :-
    read_line_to_string(user_input,S).
