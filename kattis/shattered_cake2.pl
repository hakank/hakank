% https://open.kattis.com/problems/shatteredcake
% 6s
% 1.4 Easy

% The last testcase (6/6) yields Time limit exceeded.
% But I'm not sure how to speed up this; the major thing here
% is reading the indata...
% Later:
% Change how to convert the string to numbers A and B and
% now it worked: 5.09s. -> Accepted!

% Using read_string/3 instead.
% Shorter but Memory Limit Exceeded on test 5/5!
% Well, it's quite large files...
% 
main :-
    read_string(user_input,10000000000,S),
    split_string(S,"\n ","\n ",[W0,_|Ss]),
    number_string(W,W0),
    s(Ss,0,Tot),
    Len is Tot / W,
    format('~d~n',[Len]).
s([],M,M).
s([A0,B0|Ns],M0,M) :-
    maplist(number_string,[A,B],[A0,B0]),
    M1 is M0 + A*B,
    s(Ns,M1,M).