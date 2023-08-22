% https://open.kattis.com/problems/peragrams
% 1s
% 1.9 Easy

% TODO!
% Note: It can be up to 1000 chars long strings.
% See peragrams.pi for some exploration on this..
%
main :-
    read_line_to_string(user_input,S),
    writeln(s=S),
    string_chars(S,Cs),
    writeln(Cs),
    findall(SubC,(sub_string(S, _, _,_, Sub),
                 string_length(Sub,SubLen),
                 SubLen >= 1, SubLen =< SLen2,
                 string_chars(Sub,SubC)
                 ),Subs),
    writeln(subs=Subs),
    nl.

s([]).
s([C|Cs]):-
    writeln(c=C),
    s(Cs).