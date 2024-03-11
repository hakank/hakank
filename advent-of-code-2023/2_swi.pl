/*

  Advent of Code 2023 Day 2 in SWI Prolog

  https://adventofcode.com/2023/day/2

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

go :-
        part1,
        part2,
        nl.

part1 :-
        writeln("Part 1:"),        
        File = "2.txt",
        parse_file(File,Cubes),        
        findall(Id,(member(Id=Cube,Cubes),
                    check1(Id=Cube)
                    ),
                OKs),
        sum_list(OKs,Sum),
        writeln(Sum),
        nl.

check1(Id=Cube) :-
        findall(Id,(member(C,Cube),
                    check1_(C)
                   ),Matches),
        same_length(Cube,Matches).

check1_(Cube) :-
        findall(ok,(member([Color,Num],Cube),
                   max(Color,Max),
                   Num =< Max),
                Res),
        same_length(Cube,Res).        

               
max("red",12).
max("green",13).
max("blue",14).


part2 :-
        writeln("Part 2:"),
        File = "2.txt",
        parse_file(File,Cubes),
        maplist(check2,Cubes,Prods),
        sum_list(Prods,Sum),
        writeln(Sum),
        nl.

check2(_=Cube,Prod) :-
        find_color(Cube,"red",RedMax),
        find_color(Cube,"blue",BlueMax),
        find_color(Cube,"green",GreenMax),
        Prod is RedMax*BlueMax*GreenMax.

%%
%% Find the max occurrence of this color
%%
find_color(Cube,Color,Max) :-
        findall(N,(member(Cs,Cube),
                   member([Color,N],Cs)
                  ),Nums),
        max_list(Nums,Max).


%%
%% Parse the file 
%%
parse_file(File,Cubes) :-
        read_file_to_string(File,Str,[]),
        split_string(Str,"\n", "\n",Lines),
        maplist(parse_line,Lines,Cubes).
        

parse_line(Line,Id=NumColors) :-
        split_string(Line,":"," ",[Game,Rest]),
        split_string(Game," ","", [_,Id1]),
        atom_number(Id1,Id),
        split_string(Rest,";"," ", Cubes),
        maplist(split_cube,Cubes,NumColors).

split_cube(Cube,NumColor) :-
        split_string(Cube,",", " ",Colors),
        maplist(num_color,Colors,NumColor).
        
num_color(C,[Color,Num]) :-
        split_string(C," ","",[Num1,Color]),
        atom_number(Num1,Num).
