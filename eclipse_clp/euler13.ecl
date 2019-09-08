% 
% Problem 13
% """ 
% Work out the first ten digits of the sum of the following 
% one-hundred 50-digit numbers.
%   37107287533902102798797998220837590246510135740250
%   ....
%   20849603980134001723930671666823555245252804609722
%   53503534226472524250874054075591789781264330331690")
% """
%
% Answer: 5537376230
% 0.02s
% 

go :-
        problem13.

% (0.00s)
problem13 :-
        get_lines("euler_project_problem13_numbers.txt", Lines),
        ( foreach(Line, Lines),
          foreach(N, Numbers) do
              name(Line,A), 
              iso:number_codes(N,A)
        ),
        sum(Numbers, Sum),
        number_string(Sum, SumStr),
        substring(SumStr,1,10,TenFirst),
        writeln(TenFirst).

get_lines(File, Lines) :-
	open(File, read, S),
	get_lines2(S, Lines),
	close(S).

get_lines2(S, Lines) :-
	( read_string(S, end_of_line, _, Line) ->
              Lines = [Line|Ls],
              get_lines2(S, Ls)
	;
              Lines = []
	).
