% https://open.kattis.com/problems/drmmessages
% 1
% 1.5 Easy

% Time Limit Exceeded on 26/31!
% Fixed by replacing same_length/2 with 2 x length and once(append/3)
% or perhaps the Time Limit was just a fluke...

main :-
    read_line_to_codes(user_input,S),
    length(S,Len),
    Len2 is Len // 2,
    % same_length(A,B),
    length(A,Len2),
    once(append(A,B,S)),
    s(A,AR),
    s(B,BR),
    b(AR,BR,[],R),
    format('~s~n',[R]).
main.

b([],[],S,S).
b([A|As],[B|Bs],S0,[R|S]) :-
    R is 65+(A+B) mod 26,
    b(As,Bs,S0,S).

r([],_,S,S).
r([C|Cs],N,S0,[R|S]) :-
    R is (C + N) mod 26,
    r(Cs,N,S0,S).

s(S,R) :-
    c(S,[],Ss),
    sum_list(Ss,Sum),
    r(S,Sum,[],R).

c([],S,S).
c([C|Cs],S0,[C1|S]) :-
    C1 is C-0'A,
    c(Cs,S0,S).