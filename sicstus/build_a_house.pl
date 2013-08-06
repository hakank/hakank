/*

  Simple scheduling problem in SICStus Prolog.

  From  Marriott & Stucker, "Programming in Constraints", page 17f. 
  How to build a house, simple project management.

  Compare with the folowing models:
  * Comet: http://www.hakank.org/comet/building_a_house.co
  * Gecode: http://www.hakank.org/gecode/building_a_house.cpp
  * ECLiPSe: http://www.hakank.org/eclipse/build_a_house.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go:-
        findall(LD, build(LD),L),
        write(L),nl,
        length(L, Len),
        write(len - Len),nl.


build(LD) :-
        LD = [TS,TA,TB,TC,TD,TE],
        domain(LD, 0,20),
        TS #>= 0,       % start
        TA #>= TS + 7,  % foundation
        TB #>= TA + 4,  % interior walls
        TC #>= TA + 3,  % exterior walls
        TD #>= TA + 3,  % chimney
        TD #>= TC + 2,  % roof
        % TE is the End task
        TE #>= TB + 2,  % doors
        TE #>= TD + 3,  % tiles
        TE #>= TC + 3,  % windows
        TE #=< 15,     

        % labeling([minimize(TE)],LD),
        labeling([],LD),

        write([ts-TS,ta-TA,tb-TB,tc-TC,td-TD,te-TE]),nl.


