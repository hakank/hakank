/*

  Simple scheduling problem in B-Prolog.

  From  Marriott & Stucker, "Programming in Constraints", page 17f. 
  How to build a house, simple project management.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go:-
        findall(LD, build(LD),L),
        writeln(L),
        length(L, Len),
        writeln(len:Len).
        


build(LD) :-
        LD = [TS,TA,TB,TC,TD,TE],
        LD :: 0..20,
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

        % minof(labeling(LD),TE),
        labeling(LD),

        writeln([ts-TS,ta-TA,tb-TB,tc-TC,td-TD,te-TE]).
