% https://open.kattis.com/problems/shatteredcake
% 6s
% 1.4 Easy

% The last testcase (6/6) yields Time limit exceeded.
% But I'm not sure how to speed up this; the major thing here
% is reading the indata...
% Later:
% Change how to convert the string to numbers A and B and
% now it worked: 5.09s. -> Accepted!
main :-
    read_int(W),
    read_int(_),
    read_all_int(Tot),
    Len is Tot / W,
    format('~d~n',[Len]).
main.

read_all_int(Tot) :-
    read_string(In),
    read_all(In,0,Tot).

read_all(end_of_file,S,S).
read_all(In,S0,S) :-
    split_string(In," ", "", [As,Bs]),
    number_string(A,As),
    number_string(B,Bs),    
    %  maplist(number_string,[A,B],Ss),
    % T is A*B,
    % S1 is S0+T,
    S1 is S0+A*B,
    read_string(S2),   
    read_all(S2,S1,S).

read_int(N) :-
    read_string(In),
    number_string(N,In).

read_string(S) :-
    read_line_to_string(user_input,S).
