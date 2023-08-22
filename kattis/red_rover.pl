% https://open.kattis.com/problems/redrover
% 1s
% 1.9 Easy

% Hmm, this should be that complex. It's 846 chars
% Let's see how Picat can do: red_cover.pi (447 chars)

:- set_prolog_flag(double_quotes, chars).
main :-
    read_line_to_string(user_input,S),   
    string_chars(S,Cs),
    string_length(S,SLen),
    SLen2 is SLen div 2,
    findall(SubC,(sub_string(S, _, _,_, Sub),
                 string_length(Sub,SubLen),
                 SubLen >= 1, SubLen =< SLen2,
                 string_chars(Sub,SubC)
                 ),Subs),
    s(Subs,Cs,1000,Min),
    T is min(Min,SLen),
    writeln(T).

s([],_S,M,M).
s([Sub|Subs],S,M0,M) :-
    length(Sub,SubLen),
    replace2(S,Sub,"m",Res),
    length(Res,Len),
    M1 is min(Len + SubLen,M0),
    s(Subs,S,M1,M).

replace2(String, To_Replace, Replace_With, Result) :-
    ( append([Front, To_Replace, Back], String) ->
        append([Front, Replace_With, Back], R),
        replace2(R, To_Replace, Replace_With, Result)
    ;
        Result = String
    ).
