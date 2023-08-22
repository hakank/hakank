% https://open.kattis.com/problems/t9spelling
% 1s
% 1.6 Easy

% 1 ???????
% 2 abc
% 3 def
% 4 ghi
% 5 jkl
% 6 mno
% 7 pqrs
% 8 tuv
% 9 wxyz
% 0 _

% Ah, one issue is to handle the pauses needed.
% 
% Case #3 should be
%   333666 6660 022 2777
% Not
%   33366666600222777


main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    p(Ds),
    t(1,Ss,Ds).

p(Ds) :-
    findall(C-T,(s(Cs,N),
                     length(Cs,Len),
                     between(1,Len,I),
                     nth1(I,Cs,C),
                     findall(N,between(1,I,_),T)
                     ),
            Ds).

d([],S,S).
d([X],S,[X|S]).
d([X1,X2|Xs],S0,[XX|S]) :-
    last(X1,Last),
    [First|_] = X2,
    (First == Last ->
        append(X1,[' '],XX)
    ;
        XX = X1
    ),
    d([X2|Xs],S0,S).

t(_,[],_).
t(I,[S|Ss],Ds) :-
    string_chars(S,Cs),
    findall(D,(member(C,Cs),member(C-D,Ds)),Ts),
    d(Ts,[],Ts2),
    flatten(Ts2,Ts3),
    format('Case #~d: ',[I]),
    maplist(format('~w'),Ts3),
    nl,
    I1 is I+1,
    t(I1,Ss,Ds).

s([a,b,c],'2').
s([d,e,f],'3').
s([g,h,i],'4').
s([j,k,l],'5').
s([m,n,o],'6').
s([p,q,r,s],'7').
s([t,u,v],'8').
s([w,x,y,z],'9').
s([' '],'0').