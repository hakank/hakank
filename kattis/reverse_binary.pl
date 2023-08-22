% https://open.kattis.com/problems/reversebinary
% 1s
% 1.5 Easy

% Note: b2d(L,P,..) is not really correct for common use.
% The proper way is to first reverse the list L,
% but since the assignment is to write the reverse
% version we can skip this step (and save some chars).

main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    format(codes(B),'~2R',N),
    b2d(B,1,0,Res),
    format('~d~n',[Res]).
main.

b2d([],_,S,S).
b2d([H|T],P,S0,S) :-
    P2 is P*2,
    S1 is S0 + (H-48)*P,
    b2d(T,P2,S1,S).