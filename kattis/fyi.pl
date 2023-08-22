% https://open.kattis.com/problems/fyi
% Time limit: 1s
% Diff 1.3 East

main :-
    read_line_to_codes(user_input,S),
    % 53 is '5'
    % (append([0'5,0'5,0'5],_,S) ->
    % slightly shorter
    (append([53,53,53],_,S) ->        
        writeln(1)
    ;
        writeln(0)
    ).
main.

% Using string and string_concat/3, slightly longer.
main2 :-
    read_line_to_string(user_input,S),    
    (string_concat("555",_,S) ->        
        writeln(1)
    ;
        writeln(0)
    ).
main2.
