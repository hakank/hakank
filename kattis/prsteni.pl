% https://open.kattis.com/problems/prsteni
% 1s
% 1.6 Easy

% Hmm, I got Run time error. Why?
% There's no hint in the log.
% The return value is 0 as it should.
% Perhaps Kattis' Prolog does not support regex? Yes, it does: I used it in fifty_shades_of_pink.pl
% Then what's the problem?

% Removing the regex stuff and replace with dcg and that works.

% Here's the regex check (that didn't work):
% (re_matchsub("^((\\d+)r(\\d+))",Xs,Sub) ->
%%     format('~s/~s~n',[Sub.2,Sub.3])
%% ;
%%     format('~d/1~n',[X])
%% ),

:- use_module(library(dcg/basics)).
main :-
    rs(_),rs(S),
    split_string(S," ","",Ss),
    maplist(number_string,[H|T],Ss),
    s(H,T).
main.

s(_H,[]).
s(H,[N|Ns]) :-
    X is H rdiv N,
    atom_codes(X,Xs),
    ( p([Num,Denom],Xs,[]) ->
        format('~d/~d~n',[Num,Denom])
    ;
        format('~s/1~n',[X])
    ),
    s(H,Ns).

rs(S) :-
    read_line_to_string(user_input,S).

p([N,D]) --> integer(N),[0'r],integer(D).
p([N,1]) --> integer(N).
