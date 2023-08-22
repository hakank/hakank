% https://open.kattis.com/problems/dicecup
% 1s
% 1.4 Easy

% Trying to beat Andriy Zyevakov's 176 chars solution.
% Nope, this has 297 chars. Still longer

main:-read_line_to_codes(user_input,S),append(AS,[32|BS],S),
maplist(number_string,[A,B],[AS,BS]),
findall(Sm,(b(1,A,I),b(1,B,J),Sm is I+J),C),msort(C,Cs),clumped(Cs,Cl),
sort(2,@>=,Cl,Srt),[_-Max|_]=Srt,
findall(K,(member(K-Max,Srt)),Ks),sort(Ks,KS),maplist(writeln,KS).
b(L,U,I):-between(L,U,I).
