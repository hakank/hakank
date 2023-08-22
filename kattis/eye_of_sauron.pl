% https://open.kattis.com/problems/eyeofsauron
% 1s
% 1.4 Easy
main :-
    read_line_to_codes(user_input,S),
    ( (append(Pre,[40,41|Post],S),same_length(Pre,Post),sort(Pre,[124]), sort(Post,[124])) ->
        writeln("correct")
    ;
        writeln("fix")
    ).
main.

bar --> [124]. % [0'|].
lp  --> [40]. % [0'(].
rp  --> [41]. % [0')].         

eye --> lp,rp.
eye --> bar, eye, bar.
eye --> [].