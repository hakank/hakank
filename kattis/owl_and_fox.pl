% https://open.kattis.com/problems/owlandfox
% 1s
% 1.8 Easy

% Time Limit Exceed on test 2/2
% Tried with :- table d/2. but it's still too slow on test 2/2,
% probably because of the many test cases (1..100_000).
% Is there a faster way of calculating the digit sum than number_codes+maplist+...?
% The new dsum/2 is still too slow...

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns).

s([]).
s([N|Ns]) :-
    dsum(N,Sum),
    Sum1 is Sum-1,
    ( (Sum1 =:= 1 ; Sum1 =:= 0) ->
        writeln(0)
    ;
        p(N,Sum1)
    ),
    s(Ns).

p(-1,_).
p(I,S) :-
    (dsum(I,S) ->
        writeln(I)
    ;
        I1 is I-1,
        p(I1,S)
    ).

dsum(X, X) :- X<10.
dsum(X, Y) :- X>=10, X1 is X // 10, X2 is X mod 10, dsum(X1, Y1), Y is Y1 + X2.

%% dsum(N,S) :- c(N,Ds),sum_list(Ds,S).
%% c(N,Ds) :- number_codes(N,Cs),maplist(d,Cs,Ds).
%% d(C,D) :- D is C-48.