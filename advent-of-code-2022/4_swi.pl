/*

  Advent of Code 2022 - Day 4 in SWI Prolog

    https://adventofcode.com/2022/day/4

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/


go :- 
        % File = "4_test.txt",
        File = "4.txt",        
        read_file_to_string(File,Str,[]),
        split_string(Str,"\n", "",Lines),

        member(Part,[1,2]),
        maplist(check_pairs(Part),Lines,Counts),
        sumlist(Counts,Sum),
        writeln(Sum),
        fail,
        nl.


check_pairs(Part,Line,Count) :-
        re_matchsub("^(?<a_I>\\d+)-(?<b_I>\\d+),(?<c_I>\\d+)-(?<d_I>\\d+)$",Line,Sub),
        ( ( (Part == 1, overlap_completely(Sub.a,Sub.b, Sub.c,Sub.d) ) ;
            (Part == 2, overlap_partly(Sub.a,Sub.b, Sub.c,Sub.d)) )
           -> 
           Count = 1
          ;
           Count = 0
        ).

% Part 1
overlap_completely(LowerA,UpperA, LowerB,UpperB) :-  
    ( LowerA =< LowerB, UpperA >= UpperB ) ;
    ( LowerB =< LowerA, UpperB >= UpperA ).

% Part 2
overlap_partly(LowerA,UpperA,LowerB,UpperB) :-
    LowerA =< UpperB,
    UpperA >= LowerB.

