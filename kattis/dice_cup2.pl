% https://open.kattis.com/problems/dicecup
% 1s
% 1.4 Easy

% Shorter, using msort/2 and clumped/2.
% This is 319 chars
% Andriy Zyevakov has a solution with 176 chars!
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","", Ss),
    maplist(number_string,[A,B],Ss),
    findall(Sm,(between(1,A,I),between(1,B,J),Sm is I+J),C),
    msort(C,Cs),clumped(Cs,Cl),sort(2,@>=,Cl,Srt),[_-Max|_]=Srt,
    findall(K,(member(K-Max,Srt)),Ks),sort(Ks,KS),maplist(writeln,KS).
main.
