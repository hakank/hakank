% https://open.kattis.com/problems/height
% 1s
% 2.1 Easy

% insertion sort
% Note: This is a hack since I have to reverse the list first.
% So I have probably missed something in the implementation of the sort...
% The insertion sort is from
% http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).

s([]).
s([S|Ss]) :-
    split_string(S," ","",T),
    maplist(number_string,[I|N],T),
    reverse(N,Ns), % HACK!
    i_sort(Ns,_,C),
    format("~d ~d~n",[I,C]),
    s(Ss).

i_sort(List,Sorted,C):-i_sort(List,[],Sorted,0,C).
i_sort([],Acc,Acc,C,C).
i_sort([H|T],Acc,Sorted,C0,C):-
    i(H,Acc,NAcc,0,C1),
    C2 is C0+C1,
    i_sort(T,NAcc,Sorted,C2,C).
   
i(X,[Y|T],[Y|NT],C0,C):-X>Y,C1 is C0+1,i(X,T,NT,C1,C).
i(X,[Y|T],[X,Y|T],C,C):-X=<Y.
i(X,[],[X],C,C).