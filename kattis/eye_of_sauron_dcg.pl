% https://open.kattis.com/problems/eyeofsauron
% 1s
% 1.4 Easy

% DCG version
main :-
    read_line_to_codes(user_input,S),
    (eye(S,[]) -> writeln("correct") ; writeln("fix")).
main.

eye --> [40],[41].
eye --> [124], eye, [124].
eye --> [].

% Longer version
%% bar --> [124]. % [0'|].
%% lp  --> [40]. % [0'(].
%% rp  --> [41]. % [0')].         

%% eye --> lp,rp.
%% eye --> bar, eye, bar.
%% eye --> [].