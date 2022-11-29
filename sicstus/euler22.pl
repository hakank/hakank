/*

  Euler problem 22 in SICStus Prolog

  """
  Using names.txt (right click and 'Save Link/Target As...'), a 46K 
  text file containing over five-thousand first names, begin by sorting 
  it into alphabetical order. Then working out the alphabetical value 
  for each name, multiply this value by its alphabetical position in the 
  list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, 
  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
  the list. So, COLIN would obtain a score of 938 53 = 49714.

  What is the total of all the name scores in the file?")
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

:- set_prolog_flag(double_quotes,chars).

go :-
        L = [
             % euler22a,
             euler22b
            ],
        run_problems(L).

test_split_string :-
    S = "this is a string",
    split_string(S,' ', L),
    writeln(L).

%%
%% 0.110s
%%
euler22a :-
        File = 'euler22_names.txt',
        read_file(File,Str1),
        Str1 = [Str|_], % We want the first line
        delete(Str,34,Str2),
        split_string(Str2,44, Lines1), % ','
        sort(Lines1,Lines),
        length(Lines,Len),
        findall(Score,
                (between(1,Len,I),
                 nth1(I,Lines,S1),
                 calc_score(I,S1,Score)
                ),
                Scores),
        sum_list(Scores,Sum),
        writeln(Sum).

%%
%% 0.026s
%%
euler22b :-
        File = 'euler22_names.txt',
        read_file(File,Str1),
        Str1 = [Str|_], % We want the first line
        delete(Str,34,Str2),
        split_string(Str2,44, Lines1),
        sort(Lines1,Lines),
        length(Lines,Len),
        numlist(1,Len,Is),
        maplist(calc_score,Is,Lines,Scores),
        sum_list(Scores,Sum),
        writeln(Sum).

%%
%% Calculate the score of a name * I
%%
calc_score(I,Name,Score) :-
    convert22(Name,Code),
    sum_list(Code,CodeSum),
    Score is CodeSum*I.
        
% convert a string to alpha codes (A -> 1, B -> 2, etc)
convert22(S,Codes) :-
    maplist(to_alpha_upper,S,Codes).
        

to_alpha_upper(N,Alpha) :-
        Alpha is  N - 64.

