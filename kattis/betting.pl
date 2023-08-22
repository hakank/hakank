% https://open.kattis.com/problems/betting
% 1s
% 1.3 Easy

main :-
    readln([N]),
    format('~f~n~f',[100/N,100/(100-N)]).

/*
% Compressed: 55 chars
main:-readln([N]),format('~f~n~f',[100/N,100/(100-N)]).

% 55 chars
% main:-readln([N]),T=100,format('~f~n~f',[T/N,T/(T-N)]).

*/

/*
main :-
    read_line_to_string(user_input,S),
    number_string(N1,S),
    N2 is 100 - N1,
    T1 is 100/N1,
    T2 is 100/N2,
format('~f~n~f~n',[T1,T2]).
main.
*/