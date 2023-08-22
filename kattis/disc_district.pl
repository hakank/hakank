% https://open.kattis.com/problems/discdistrict
% 3s
% 1.8 Easy

% This works (220 chars) but there must be a
% much simpler way since the shortest programs are about 8 chars
% (it's PHP and Ruby that's are top 10)
%
% In disc_district2.pl it works to output "1 S"
% but it takes 61 chars!
% My Python3 program disc_disctrict takes 19 chars

% This Picat program takes 32 chars:
%   println("1 "++read_file_chars())

:- use_module(library(clpfd)).
main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    [A,B] ins 1..N,
    C #= A+B,    
    A^2+B^2 #>= N*N,
    labeling([min(C)],[A,B]),
    format("~d ~d~n",[A,B]).

/*
% Compressed: 181 chars
:- use_module(library(clpfd)).
main:-read_line_to_string(user_input,S),number_string(N,S),
[A,B] ins 1..N,C #= A+B,A^2+B^2 #>= N*N,labeling([min(C)],[A,B]),format("~d ~d~n",[A,B]).


*/