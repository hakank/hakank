% https://open.kattis.com/problems/piglatin
% 3s
% 2.2 Easy

/*
 * If a word begins with a consonant, take all of the letters before the first 
   vowel and move them to the end of the word, then add ay to the end of the word. 
   Examples: pig -> igpay, there ->  erethay.

 * If a word begins with a vowel (a, e, i, o, u, or y), simply add yay to the end 
   of the word. For this problem, y is always a vowel. 
   Examples: and -> andyay, ordinary ->  ordinaryyay.

*/

% 581 chars
main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s([S|Ss]) :-
    split_string(S," ","",Q),
    maplist(string_chars,Q,T),
    p(T),
    nl,
    s(Ss).
p([]).
p([[H|T]|Ss]) :-
    V=[a,e,i,o,u,y],
    (memberchk(H,V)->
        append([H|T],[y,a,y],R)
    ; 
        f([H|T],V,[],P),
        append(P,Rest,[H|T]),
        append(Rest,[P,a,y],R0),
        flatten(R0,R)
    ),
    format("~s ",[R]),
    p(Ss).   
f([],S,S).
f([H|T],V,S0,S) :-
    (memberchk(H,V) ->
        S=S0
    ;
        append(S0,[H],S1),
        f(T,V,S1,S)
    ).

/*
% Compressed: 420 chars
main:-read_string(user_input,_,S),split_string(S,"\n","\n",Ss),s(Ss).
s([]). s([S|Ss]):-split_string(S," ","",Q),maplist(string_chars,Q,T),p(T),nl,s(Ss).
p([]). p([[H|T]|Ss]):-V=[a,e,i,o,u,y],(memberchk(H,V)->append([H|T],[y,a,y],R)
;f([H|T],V,[],P),append(P,Rest,[H|T]),append(Rest,[P,a,y],R0),flatten(R0,R)),
format("~s ",[R]),p(Ss).   
f([],S,S). f([H|T],V,S0,S):-(memberchk(H,V)->S=S0;append(S0,[H],S1),f(T,V,S1,S)).

*/