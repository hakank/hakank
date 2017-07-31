/*

  Nadel's construction problem in SICStus Prolog.

  From Rina Dechter "Constraint Processing", page 5.
  Attributes the problem to
  B.A. Nadel "Constraint satisfaction algorithms" (1989).
  """
  * The recreation area should be near the lake.
  
  * Steep slopes are to be avoided for all but the recreation area.
  * Poor soil should be avoided for those developments that 
    involve construction, namely the apartments and the family houses.
  
  * The highway, being noisy, should not be near the apartments, 
    the housing, or the recreation area.
  
  * The dumpsite should not be visible from the apartments, 
    the houses, or the lake.
  
  * Lots 3 and 4 have bad soil.
  * Lots 3, 4, 7, and 8 are on steep slopes .
  * Lots 2, 3, and 4 are near the lake.
  * Lots 1 and 2 are near the highway.
  """

  Comment: 
  I have not found any model that satisfies all the constraints.
  However this "soft" version counts the broken constraints
  and minimizes to 1 broken constraint.
  
  The model (which - of course - could be erroneous) generates 28 different 
  models. The broken constraints are either
    - steep_slopes constraints or
    - near_dump constraints.
 

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/nadel.mzn
  * Gecode  : http://www.hakank.org/gecode/nadel.cpp


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        findall([Developments,Broken,TotalCount],
                 nadel(Developments,Broken,TotalCount),
                 List),
        length(List,Len),
        ( foreach([Developments,Broken,TotalCount], List),
          fromto(AllBroken,[Broken|In],In,[]) do
              write(developments:Developments),nl,
              format('Constraints broken: ~w\n',[Broken]),
              format('Total broken constraints: ~d\n',[TotalCount]),nl
        ),
        nl,
        write(length:Len),nl,

        write('\nWhich constraints was broken in the above list?\n'),
        transpose(AllBroken,AllBrokenTransposed),
        ( foreach(B,AllBrokenTransposed),
          count(C,1,_) do
              sum(B,#=,Occurrences),
              format('Contraint ~d has ~d occurences\n',[C,Occurrences])
        ),
        nl,
        fd_statistics.


nadel(Developments, Broken, TotalCount) :-

        % Near lots
        % * Lots 3 and 4 have bad soil.
        % * Lots 3, 4, 7, and 8 are on steep slopes .
        % * Lots 2, 3, and 4 are near the lake.
        % * Lots 1 and 2 are near the highway.
        
                        % 1, 2, 3, 4, 5, 6, 7, 8
        BadSoil     =  [0, 0, 1, 1, 0, 0, 0, 0],
        SteepSlopes =  [0, 0, 1, 1, 0, 0, 1, 1],
        NearLake    =  [0, 1, 1, 1, 0, 0, 0, 0],
        NearHighway =  [1, 1, 0, 0, 0, 0, 0, 0],
        
        % neighborhood matrix (for the dump placement)
        NearLots =  % 1  2  3  4  5  6  7  8  
                    [[0, 1, 0, 0, 1, 0, 0, 0], % 1
                     [1, 0, 1, 0, 0, 1, 0, 0], % 2 
                     [0, 1, 0, 1, 0, 0, 1, 0], % 3 
                     [0, 0, 1, 0, 0, 0, 0, 1], % 4
                     [1, 0, 0, 0, 0, 1, 0, 0], % 5
                     [0, 1, 0, 0, 1, 0, 1, 0], % 6
                     [0, 0, 1, 0, 0, 1, 0, 1], % 7
                     [0, 0, 0, 1, 0, 0, 1, 0]], % 8

        length(NearLots,N), % number of lots        
        
        % the development to place in one of the lots
        Developments = [Recreation, Apartments, Houses, Cemetery,
                        Dump],
        domain(Developments, 1,N),

        C = 13, % number of constraints
        length(Broken,C),
        domain(Broken,0,1), % indicator of broken constraint
        Broken = [Broken1,Broken2,Broken3,Broken4,Broken5,Broken6,
                  Broken7,Broken8,Broken9,Broken10,Broken11,Broken12,
                  Broken13],

        sum(Broken,#=,TotalCount),
        TotalCount #=< 1, % for findall

        all_different(Developments),

        % * The recreation area should be near the lake.
        element(Recreation,NearLake,NearLakeRecreation),
        (NearLakeRecreation #= 1 #<=> Broken1 #= 0),
        
        % * Steep slopes are to be avoided for all but the recreation
        %   area.
        element(Apartments,SteepSlopes,SteepSlopesApartments),
        element(Houses,SteepSlopes,SteepSlopesHouses),
        element(Cemetery,SteepSlopes,SteepSlopesCemetry),
        element(Dump,SteepSlopes,SteepSlopesDump),
        (SteepSlopesApartments #= 0 #<=> Broken2 #= 0),
        (SteepSlopesHouses     #= 0 #<=> Broken3 #= 0),
        (SteepSlopesCemetry    #= 0 #<=> Broken4 #= 0),
        (SteepSlopesDump       #= 0 #<=> Broken5 #= 0),

        % * Poor soil should be avoided for those developments that 
        %   involve construction, namely the apartments and the family
        %   houses.
        element(Apartments,BadSoil,BadSoilApartments),
        element(Houses,BadSoil,BadSoilHouses),
        (BadSoilApartments #= 0 #<=> Broken6 #= 0 ),
        (BadSoilHouses     #= 0 #<=> Broken7 #= 0 ),
        
        % * The highway, being noisy, should not be near the apartments, 
        %   the housing, or the recreation area.
        element(Apartments,NearHighway,NearHighwayApartments),
        element(Houses,NearHighway,NearHighwayHouses),
        element(Recreation,NearHighway,NearHighwayRecreation),
        (NearHighwayApartments #= 0 #<=> Broken8 #= 0),
        (NearHighwayHouses     #= 0 #<=> Broken9 #= 0),
        (NearHighwayRecreation #= 0 #<=> Broken10 #= 0),
        
        % * The dumpsite should not be visible from the apartments, 
        %   the houses, or the lake.
        
        % not near the lake
        element(Dump,NearLake, NearLakeDump),
        (NearLakeDump #= 0 #<=> Broken11 #= 0),
        
        % not near the house 
        matrix_element(NearLots,Dump,Houses,NearLotsDumpHouses),
        matrix_element(NearLots,Houses,Dump,NearLotsHousesDump),
        (
            (NearLotsDumpHouses #= 0 #/\ NearLotsHousesDump #= 0)
                  #<=> 
            Broken12 #= 0
        ), 

        % not near the apartments  
        matrix_element(NearLots,Dump,Apartments,NearLotsDumpApartments),
        matrix_element(NearLots,Apartments,Dump,NearLotsApartmentsDump),
        (
            (NearLotsDumpApartments #= 0 #/\ NearLotsApartmentsDump #= 0)
        #<=> Broken13 #= 0
        ),
        
        % search
        append(Developments,Broken,Vars),
        labeling([], Vars).


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).
        
