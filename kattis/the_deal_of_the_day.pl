% https://open.kattis.com/problems/thedealoftheday
% 1s
% 2.1 Easy

% subseq/2 from Peter Ludemann's countdown program
% https://github.com/kamahen/nerdle/blob/main/countdown.pl

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n",Ss),
    maplist(number_string,Ns1,Ss),
    length(Ns,10),
    append(Ns,[N],Ns1),
    findall(V,(between(1,10,I),nth1(I,Ns,V),V>0),Vs),
    length(X,N),
    findall(P,(subseq(Vs,X),p(X,1,P)),Xs),
    sum_list(Xs,Sum),
    writeln(Sum).

p([],P,P).
p([H|T],P0,P) :-
    P1 is P0*H,
    p(T,P1,P).
 
subseq(List, Subseq) :-
    Subseq = [_|_],
    subseq_(List, Subseq).
subseq_([], []).
subseq_([Head|Tail], [Head|SubTail]) :-
    subseq_(Tail, SubTail).
subseq_([_|Tail], SubSequence) :-
    subseq_(Tail, SubSequence).
