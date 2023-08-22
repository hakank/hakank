% https://open.kattis.com/problems/fizzbuzz
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S, " ", "", Ss),
    maplist(number_string,[Fizz,Buzz,N],Ss),
    fizzbuzz(1,N,Fizz,Buzz).
main.

fizzbuzz(N2,N,_Fizz,_Buzz) :- N2 > N .
fizzbuzz(C,N,Fizz,Buzz) :-
    (C mod Fizz =:= 0 -> A = "Fizz" ; A = ""),
    (C mod Buzz =:= 0 -> B = "Buzz" ; B = ""),
    string_concat(A,B,Res),
    (Res == "" ->
        writeln(C)
    ;
        writeln(Res)
    ),    
    C2 is C+1,
    fizzbuzz(C2,N,Fizz,Buzz).