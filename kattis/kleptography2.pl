% https://open.kattis.com/problems/kleptography
% 1s
% 1.5 Easy

% c(S,Len,Res)
% Res is the string S copied to be of the exact length Len

% Using read_string/3 instead.
% Cf the_key_to_cryptography.pl
% I thought that it would work to reverse the stuff in the_key_to_cryptography.pl
% but that doesn't work...

main :-
    read_string(user_input,100000000,S),
    writeln(s=S),
    split_string(S,"\n ","\n ",[_,_|Ss]),
    maplist(string_codes,Ss,[K,C]),
    writeln(ss=Ss),
    reverse(C,CRev),
    reverse(K,KRev),
    d(CRev,1,KRev,[],R),
    reverse(R,Rev),
    format('~s~n',[Rev]).

d([],_,_,S,S).
d([C|Cs],I,Key,S0,[D2|S]) :-
    nth1(I,Key,K),
    D is (C-K) mod 26,
    D2 is D+0'A,
    append(Key,[D2],Key2),
    I1 is I+1,    
    d(Cs,I1,Key2,S0,S).
