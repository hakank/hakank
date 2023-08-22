% https://open.kattis.com/problems/welcomeeasy
% 1s
% 2.1 Easy

% Counting subsequences
% DP approach from
% https://www.techiedelight.com/count-number-times-pattern-appears-given-string-subsequence/
% (See welcome_easy.pi for a Picat variant)

% Is this fast enough?
% It's accepted but it's slower than welcome_easy.pl (0.10s vs 0.07s)
% and it's longer (837chars vs 640 chars).

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(1,Ss).
s(_,[]).
s(I,[S|Ss]) :-
    string_chars(S,C),
    length(C,CLen),
    T = [w,e,l,c,o,m,e,' ',t,o,' ',c,o,d,e,' ',j,a,m],
    count(C,T,CLen,19,Count),
    R0 is Count mod 1000,
    f(R0,R),
    format("Case #~d: ~s~n",[I,R]),
    I1 is I+1,
    s(I1,Ss).

count([X|_],[Y|_],1,1,C) :-
    (X == Y -> C is 1 ; C is 0).
count(_,_,0,_,0).
count(_,_,_,0,1).
count(_,_,M,N,0) :- N > M.
count(X,Y,M,N,C) :-
    nth1(M,X,XM),
    nth1(N,Y,YN),    
    M1 is M-1,
    (XM == YN -> N1 is N-1, count(X,Y,M1,N1,C1);C1 = 0),
    count(X,Y,M1,N,C2),
    C is C1+C2.
    
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