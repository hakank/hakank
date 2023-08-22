% https://open.kattis.com/problems/keytocrypto
% 1s
% 1.7 Easy

% Cf the Picat program the_key_to_cryptography.pi
% which is shorter (198 chars) and neater.

% 336 chars uncompressed

% Trying another approach, skipping nth1/3,
% 222 chars compressed

main:-rc(C),rc(K),d(C,1,K,[],R),format('~s~n',[R]).
d([],_,_,S,S).
d([C|Cs],I,[K|Key],S0,[D2|S]):-D is (C-K) mod 26,D2 is D+0'A,
append(Key,[D2],Key2),I1 is I+1,d(Cs,I1,Key2,S0,S).
rc(S):-read_line_to_codes(user_input,S).
