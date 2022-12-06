/*

  Advent of Code 2022 - Day 6 in SWI Prolog

  https://adventofcode.com/2022/day/6


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

go :- 
        % File = "6_test.txt",
        File = "6.txt",
        read_file_to_string(File,Line,[]),
        member(Part,[1,2]),
        (Part == 1 -> Len = 4 ; Len = 14),
        ( (sub_string(Line,Before,Len,_After,S),string_chars(S,Cs),all_diff(Cs) ) ->
          Pos is Before + Len,
          writeln(Pos)
        ;
          fail
        ),
        fail.


all_diff([]).
all_diff([_]).
all_diff([H|T]) :-
        \+ memberchk(H,T),
        all_diff(T).