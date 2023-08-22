% https://open.kattis.com/problems/schoolspirit
% 1s
% 1.7 Easy

main :-read_string(user_input,100000,S),
split_string(S,"\n ","\n ",Ss),
maplist(number_string,[N|Ns],Ss),N1 is N-1,
s(Ns,0,N1,0,C),R1 is C/5,writeln(R1),
N2 is N-1,findall(G2,(select(_,Ns,Ns2),s(Ns2,0,N2,0,G),G2 is G/5),Gs),
sum_list(Gs,GSum),R2 is GSum /N,writeln(R2).
s([],I,Max,S,S):-I>=Max,!.
s([N|Ns],I,Max,S0,S):-C is N*(4/5)^I,S1 is S0 + C,I1 is I+1,s(Ns,I1,Max,S1,S).