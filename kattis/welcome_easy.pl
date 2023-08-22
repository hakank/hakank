% https://open.kattis.com/problems/welcomeeasy
% 1s
% 2.1 Easy

% Counting subsequences

% Nice that the append/3 approch works.
% But it's the easy variant of this problem.


main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(1,Ss).
s(_,[]).
s(I,[S|Ss]) :-
    string_chars(S,C),
    findall(1, append(
        [_,[w],_,[e],_,[l],_,[c],_,[o],_,[m],_,[e],_,[' '],_,
         [t],_,[o],_,[' '],_,[c],_,[o],_,[d],_,[e],_,[' '],_,[j],_,[a],_,[m],_],
        C),
        Ts),
    length(Ts,Len),
    R0 is Len mod 1000,
    f(R0,R),
    format("Case #~d: ~s~n",[I,R]),
    I1 is I+1,
    s(I1,Ss).

f(Len,L) :-
    number_chars(Len,Cs),
    length(Cs,CLen),
    D is 4-CLen,
    (D > 0 ->        
        findall('0',between(1,D,_),F),
        append(F,Cs,L)
    ;
        L = Cs
    ).