% https://open.kattis.com/problems/kafkaesque
% 1s
% 2.0 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns,0,N),
    writeln(N).
s([],L,L).
s([_],L,L1) :- L1 is L+1.
s([A,B|T],L0,L) :- (A < B -> L1 is L0 ; L1 is L0+1 ), s([B|T],L1,L).

