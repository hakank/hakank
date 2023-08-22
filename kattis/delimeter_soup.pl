% https://open.kattis.com/problems/delimitersoup
% 1s
% 2.1 Easy

% I'm testing a non DCG version first.

% Darn, I cannot use readln/2 since it removes " " which are imporant here.

% 574 char (First in Prolog top list)
main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_,Ss]),
    string_chars(Ss,Cs),
    (s(Cs,0,[],_) ->
        writeln("ok so far")
    ;
        true
    ).

s([],_,R,R).
s([C|Cs],I,R0,R) :-
    (C==' ' ->
        R1 = R0
    ;
        (memberchk(C,['[','{','(']) ->
            append([C],R0,R1)
        ;
            c(O,C),
            ( [O|R1]=R0 ->
                true
            ;
                format("~w ~d~n",[C,I]),
                fail
            )
        )
    ),
    I1 is I+1,
    s(Cs,I1,R1,R).
c('[',']').
c('{','}').
c('(',')').

/*
% Compressed: 342 chars. I haven't submitted this, though...
main:-read_string(user_input,_,S),split_string(S,"\n","\n",[_,Ss]),string_chars(Ss,Cs),
(s(Cs,0,[],_)->writeln("ok so far");true).
s([],_,R,R). s([C|Cs],I,R0,R):-
(C==' '->R1=R0;(memberchk(C,['[','{','('])->append([C],R0,R1);c(O,C),
([O|R1]=R0->true;format("~w ~d~n",[C,I]),fail))),I1 is I+1,s(Cs,I1,R1,R).
c('[',']'). c('{','}'). c('(',')').

*/