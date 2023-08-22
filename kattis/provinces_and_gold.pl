% https://open.kattis.com/problems/provincesandgold
% 1s
% 1.5 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ss),
    maplist(number_string,[G,Sr,C],Ss),
    Cs=[3,2,1],
    sp([G,Sr,C],Cs,0,BP),
    findall(Cd-Ct,(v(Cd,Ct,_),Ct =< BP),VC),
    (VC \= [], [V-_|_] = VC ; V = []),
    findall(Cd-Ct,(t(Cd,Ct,_),Ct=<BP),TC),
    (TC \= [], [T-_|_] = TC ; T = []),
    w(V,T).
main.

w(V,T) :- ((V\=[],T\=[])->format('~s or ~s~n',[V,T]);
(V\=[]->writeln(V);writeln(T))).
v('Province',8,6). v('Duchy',5,3). v('Estate',2,1).
t('Gold',6,3). t('Silver',3,2). t('Copper',0,1).
sp([],[],R,R).
sp([X|Xs],[Y|Ys],R0,R) :-  R1 is 0 + X*Y,
    sp(Xs,Ys,R1,R).
