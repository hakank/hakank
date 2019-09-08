%
% Problem 22
%
% """
% Using names.txt (right click and 'Save Link/Target As...'), a 46K 
% text file containing over five-thousand first names, begin by sorting 
% it into alphabetical order. Then working out the alphabetical value 
% for each name, multiply this value by its alphabetical position in the 
% list to obtain a name score.
%
% For example, when the list is sorted into alphabetical order, COLIN, 
% which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
% the list. So, COLIN would obtain a score of 938 53 = 49714.
%
% What is the total of all the name scores in the file?")
% """
%
% Answer: 871198282
%
% (0.01s)

go :-
  problem22.

problem22 :-
        get_lines("euler_names.txt", [Str]),
        split_string(Str, ",","\",",NameListUnsorted),
        sort(NameListUnsorted, NameList),
        length(NameList,Len),
        ( for(I,1,Len),
          foreach(Name, NameList),
          fromto(0,In,Out,Total) do
              p22_convert(Name, S),
              Out is In + S*I
        ),
        writeln(Total).

% Convert each letter to numbers, A->1, B->2, etc.
p22_convert(Name,NameSum) :- 
        name(Name, Str),
        ( foreach(Letter, Str),
          fromto(0,In,Out,NameSum) do
              Out is In + Letter - 64
        ).

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
