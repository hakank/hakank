% https://open.kattis.com/problems/owlandfox
% 1s
% 1.8 Easy


/*

  Generating 100000 random numbers between 1..100000:
   $ swipl -g "between(1,100000,_),random(1,100000,X),writeln(X),fail" > owl_and_fox3.inp

  Observation: 
    For N=1520 the answer is 1510 
  i.e. the last non-zero is decremented by one (using some modulo stuff).

  This is much faster (0.37s vs 1.04s on all 1..100000) and is accepted.

*/
% 324 chars
main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns).

m(I,N,N2) :-
    M is N mod 10^I,
    (M > 0 ->
        N2 is N-(10^(I-1))
    ;
        I1 is I+1,
        m(I1,N,N2)
    ).

s([]). 
s([N|Ns]) :-
    m(1,N,N2),
    writeln(N2),
    s(Ns).


/*
  % Compressed: 235 chars
main:-read_string(user_input,100000000,S),split_string(S,"\n","\n",[_|Ss]),maplist(number_string,Ns,Ss),s(Ns).
m(I,N,N2):-M is N mod 10^I,(M>0->N2 is N-(10^(I-1));I1 is I+1,m(I1,N,N2)).
s([]). s([N|Ns]):-m(1,N,N2),writeln(N=N2),s(Ns).

*/