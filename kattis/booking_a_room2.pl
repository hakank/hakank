% https://open.kattis.com/problems/bookingaroom
% 1s
% 1.8 Easy
% Shorter
main :-
    ga(S),ss(S,"\n",Ss),[RS|Ns] = Ss,ss(RS," ",[RR,SS]),
    m([RN,_SN],[RR,SS]),m(Rs,Ns),
    findall(F, (between(1,RN,F),not(memberchk(F,Rs))),Fr),
    length(Fr,L),
    (L>0->[H|_]=Fr,writeln(H);writeln("too late")).
main.
ss(S,C,Ss):-split_string(S,C,"",Ss).
m(N,S):-maplist(number_string,N,S).
ga(S):-see(user_input),a(C),append(CC,[10],C),string_codes(S,CC),seen.
a([C|R]):-get0(C), C \== -1, !, a(R). a([]).
