% https://open.kattis.com/problems/jackolanternjuxtaposition
% Time limit: 1s
% Diff 1.3 Easy

%
% Here's a Picat version in 53 chars:
% Picat> println(read_line(stdin).split(" ").map(to_int).prod)
% 2 2 2
% 8
% The current shortest solution is a Ruby program in 17 chars.
%
main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ss),
    maplist(number_string,Ns,Ss),
    foldl(mult,Ns,1,P),
    writeln(P).
main.

mult(X,Y,Z) :- Z is X*Y.