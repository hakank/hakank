% https://open.kattis.com/problems/jobexpenses
% 1s
% 1.4 Easy

% Ah, the number of entries could be 0 (and with no second line, I assume).
% And using a compressed version to be shorter than Andriy Zyevakov's 291 chars version.
main :-rs(NumS),number_string(Num,NumS),(Num=:=0->writeln(0);
rs(S),split_string(S," ","",Ss),maplist(number_string,Ns,Ss),findall(E,(member(E,Ns),E<0),Es),
sumlist(Es,Sum),AbsSum is abs(Sum),   writeln(AbsSum)).
main.
rs(S):-read_line_to_string(user_input,S).
