% https://open.kattis.com/problems/keytocrypto
% 1s
% 1.7 Easy

% Cf the Picat program the_key_to_cryptography.pi
% which is shorter (198 chars) and neater.

% 336 chars uncompressed
main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    maplist(string_codes,Ss,[C,K]),
    d(C,1,K,[],R),    
    format('~s~n',[R]).
d([],_,_,S,S).
d([C|Cs],I,Key,S0,[D2|S]) :-
    nth1(I,Key,K),
    D is (C-K) mod 26,
    D2 is D+0'A,
    append(Key,[D2],Key2),
    I1 is I+1,    
    d(Cs,I1,Key2,S0,S).
