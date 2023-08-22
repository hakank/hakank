% https://open.kattis.com/problems/simplearithmetic
% Time limit: 1s
% Difficulty: 1.2-4.8 Medium

% Accepted 50/100
% Some were wrong answers.
% First I missed that when C == 1 then it should be an
% integer solution, but then there's other problems:
% SWI Prolog cannot handle test case 4:
%   ?- D is 123456789*987654321/7, format('~30f~n',[D]).
%   17418947301805038.000000000000000000000000000000
%   D = 1.7418947301805038e+16.
% It should be 17418947301805038.428571428571428571
% It's the same with Python3
%   >>> print(f"{123456789*987654321/7:20.14f}")
%   17418947301805038.00000000000000
%
% And Picat.
main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ns0),
    maplist(number_string,[A,B,C],Ns0),
    D is (A*B)/C,     
    (C =:= 1 ->
        writeln(D)
    ;
        format('~30f~n',D)
    ).
main.

