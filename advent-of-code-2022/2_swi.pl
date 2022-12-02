/*

  Advent of Code 2022 - Day 2  in SWI Prolog

  https://adventofcode.com/2022/day/2

  This is the same approach as in my Picat solution 1.pi go2/0.
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

go :- 
        % File = "2_test.txt",
        File = "2.txt",
        read_file_to_string(File,Str,[]),
        split_string(Str,"\n", "",Lines),

        % Convert to "Other Me" -> [Other,Me]
        maplist(ss,Lines,Lines2),
        
        % Part 1
        calc_points1(Lines2,0,Points1),
        writeln(Points1),

        % Part 2
        calc_points2(Lines2,0,Points2),
        writeln(Points2),
        
        nl.

% Wrapper for maplist
ss(Line,[Other,Me]) :-
        split_string(Line," ",[],[Other,Me]).

% Part1
calc_points1([],P,P).
calc_points1([[Other,Me]|Lines],Points0,Points) :-
        map_other(Other,O),
        map_me1(Me,M),
        
        ( (beats(M,O),P2=6)
        ;
          (draw(M,O), P2=3)
        ;
          (loses(M,O),P2=0)
        ),
        
        map_points(M,P1),
        Points1 is Points0 + P1 + P2,
        calc_points1(Lines,Points1,Points).

% Part 2
calc_points2([],P,P).
calc_points2([[Other,Me]|Lines],Points0,Points) :-
        map_other(Other,O),
        map_me2(Me,M2),
        
        call(M2,Pick,O),
        map_points(M2,P2),
        map_points(Pick,P1),
        
        Points1 is Points0 + P1 + P2,
        calc_points2(Lines,Points1,Points).

% The lookup tables/maps
map_other("A","Rock"). map_other("B","Paper"). map_other("C","Scissors").

map_me1("X","Rock"). map_me1("Y","Paper"). map_me1("Z","Scissors").

map_me2("X",loses). map_me2("Y",draw). map_me2("Z",beats).

map_points("Rock",1). map_points("Paper",2).  map_points("Scissors",3).

map_points(beats,6). map_points(draw,3). map_points(loses,0).   

beats("Rock","Scissors").
beats("Scissors","Paper").
beats("Paper","Rock").

loses(X,Y) :- beats(Y,X).

draw(X,X).