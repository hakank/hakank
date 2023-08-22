% https://open.kattis.com/problems/breakingbranches
% 1s
% 1.6 Easy

% Python3: 52 chars but not small enough for Top 10 (26..45 chars)
% print("Alice\n1\n"if int(input())%2==0 else "Bob\n")

% 47 chars
% print("Alice\n1"if int(input())%2==0 else "Bob"

% 44 chars: Top 10 place 10 in Python3 short list
% print(["Bob","Alice\n1"][int(input())%2==0])

% 157 chars (using readln/1 in breaking_branches2.pl: 69 chars)
main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    (N mod 2 =:= 0 ->
        format("Alice~n1~n")
    ;
        writeln("Bob")
    ).

