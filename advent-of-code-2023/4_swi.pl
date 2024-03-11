/*

  Advent of Code 2023 Day 4 in SWI Prolog

  https://adventofcode.com/2023/day/4

  This is a port of my Picat solution 4.pi

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

go =>
        File = "4.txt",
        parse_file(File,Scores,Wins),

        % Part1
        sum_list(Scores,TotalScores),
        writeln(TotalScores),

        %% Part 2
        part2(Wins,Part2),
        sum_list(Part2,Part2Sum),
        writeln(Part2Sum),
        nl.

part2(Wins,Part2) =>
        length(Wins,Len),
        findall(1,between(1,Len,_),Times), % start with 1 copy each
        part2_(Wins,Times,Len,Part2).

part2_(Wins,Times,Len,TimeRes) =>
        part2_loop(1,Len,Wins,Times,TimeRes).

part2_loop(I,Len,_Wins,Times,Times) :-
        I > Len.
part2_loop(I,Len,Wins,Times,TimesRes) :-
        nth1(I,Wins,WinsI),
        nth1(I,Times,TimesI),
        % Foreach J in I+1..Wins[I] update Times[J]
        I1 is I+1,
        WinsLoop is I+WinsI,
        add_times(I1,Len,WinsLoop,TimesI,Times,TimesUp),
        part2_loop(I1,Len,Wins,TimesUp,TimesRes).
        
add_times(J,Len,WinsLoop,_TimesI,TimesUp,TimesUp) :- (J > WinsLoop ; J > Len).
add_times(J,Len,WinsLoop,TimesI,Times0,Times) :-
        J =< WinsLoop,
        nth1(J,Times0,TimesJ),
        TimesJNew is TimesJ + TimesI,
        update_times(J,Len,Times0,TimesJNew,Times1),
        J1 is J+1,
        add_times(J1,Len,WinsLoop,TimesI,Times1,Times).

% Update: Times0[J] = NewT
update_times(J,Len,Times0,NewT,TimesNew) =>
        findall(V,(between(1,Len,I),
                   (I == J -> V = NewT 
                   ;
                    nth1(I,Times0,V)
                   )
                  ),TimesNew).

% Parsing the file
parse_file(File,Scores,Wins) =>
        read_file_to_string(File,Str,[]),
        split_string(Str,"\n", "\n",Lines),
        maplist(parse_lines,Lines,Scores,Wins).

parse_lines(LineS,Score,Wins) =>
        split_string(LineS,":", " ",[_,Numbers]),
        split_string(Numbers,"|", "",[WinningsS,MyNumbersS]),
        split_string(WinningsS," "," ",Winnings),
        split_string(MyNumbersS," "," ",MyNumbers),
        scores_and_wins(MyNumbers,Winnings,Score,Wins).

scores_and_wins(MyNumbers,Winnings,Score,Wins) =>
        score_and_wins_(MyNumbers,Winnings,0,Score,0,Wins).

score_and_wins_([],_,Score,Score,Wins,Wins).
score_and_wins_([N|T],Winnings,Score0,Score,Wins0,Wins) :-
        (memberchk(N,Winnings) ->
         (Score0 == 0 -> Score1 = 1  ; Score1 is Score0*2),
         Wins1 is Wins0 + 1
        ;
         Score1 = Score0,
         Wins1 = Wins0
        ),
        score_and_wins_(T,Winnings,Score1,Score,Wins1,Wins).

                 