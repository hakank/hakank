% https://open.kattis.com/problems/discdistrict
% 3s
% 1.8 Easy

% This is correct: 61 chars
main:-read_line_to_string(user_input,S),format("1 ~s~n",[S]).

/*
% 154 chars
% Shorter version. It works but it's slower than the CP version (0.33s vs 0.03s)
main2 :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    between(1,N,A),between(1,N,B),D is A^2+B^2,D>N*N,
    format("~d ~d~n",[A,B]).

% 161 chars and slower 0.74s
main1:-read_line_to_string(user_input,S),number_string(N,S),s(1,1,N).
s(A,B,N):-(A^2+B^2 > N*N->format("~d ~d~n",[A,B]);A1 is A+1,s(A1,B,N);B1 is B+1,s(A,B1,N)).

*/