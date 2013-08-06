/*

  Four Islands puzzle (Dell Logic Puzzles) in SICStus Prolog.

 http://brownbuffalo.sourceforge.net/FourIslandsClues.html
  """
  Title: Four Islands
  Author: Humphrey Dudley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1
  
  A tiny nation in the South Pacific contains four islands connected by bridges
  as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
  boasts a different primary export (alabaster, bananas, coconuts, and durian
  fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
  stadium, and koala preserve). Can you find the name, export, and tourist 
  attraction of each island on the map?
  
    N
  W   E     *compass directions
    S
  
  A, B, C, D are the islands
  
  (A) -- (B)
   |      |
   |      |
  (C) -- (D)
  
  
  1. The island noted for its koala preserve is due south of Pwana.
  2. The island with the largest alabaster quarry is due west of Quero.
  3. The island with the resort hotel is due east of the one that exports 
     durian fruit.
  4. Skern and the island with the jai alai stadium are connected by a 
     north-south bridge. 
  5. Rayou and the island that exports bananas are connected by an east-west
     bridge.
  6. The islands noted for the South Pacific's largest ice skating rink and 
     for the jai alai stadium are not connected by a bridge.
  
  Determine: Island island -- Island name -- Export -- Tourist Attraction
  """


  Compare with other models:
  * MiniZinc: http://www.hakank.org/minizinc/four_island.mzn 
  * Comet   : http://www.hakank.org/comet/four_islands.mzn 
  * ECLiPSe : http://www.hakank.org/eclipse/four_islands.ecl

  And also the F1 model
  http://www.f1compiler.com/samples/Four%20Islands.f1.html


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 4,
        Range = 1..N,

        A = 1,
        B = 2,
        C = 3,
        D = 4,

        Island = [Pwana, Quero, Rayou, Skern],
        domain(Island,1,N),
        IslandS = ['Pwana', 'Quero', 'Rayou', 'Skern'],

        Export = [Alabaster, Bananas, _Coconuts, DurianFruit],
        domain(Export,1,N),
        ExportS = ['alabaster', 'bananas', 'coconuts', 'durian_fruit'],
        
        Attraction = [ResortHotel, IceSkatingRink, JaiAlaiStadium,
                        KoalaPreserve],
        domain(Attraction,1,N),
        AttractionS = ['resort_hotel', 'ice_skating_rink',
                         'jai_alai_stadium', 'koala_preserve'],

        
        all_different(Island),
        all_different(Export),
        all_different(Attraction),


        % 1. The island noted for its koala preserve is due south of Pwana.
        (
         (Pwana #= A #/\ KoalaPreserve #= C)
         #\/
         (Pwana #= B#/\KoalaPreserve #= D)
         ),

        % 2. The island with the largest alabaster quarry is due west of Quero.
        ( 
         (Alabaster #= A#/\Quero #= B) 
         #\/
         (Alabaster #= C#/\Quero #= D) 
          ),

        % 3. The island with the resort hotel is due east of the one 
        %    that exports durian fruit.
        ( 
         (DurianFruit #= A#/\ResortHotel #=  B )
         #\/
         ( DurianFruit #= C#/\ResortHotel #=  D)
          ),

        % 4. Skern#/\the island with the jai alai stadium are connected by a 
        %    north-south bridge. 
        (
         (Skern #= A#/\JaiAlaiStadium #= C) 
         #\/
         (Skern #= C#/\JaiAlaiStadium #= A) 
         #\/
         (Skern #= B#/\JaiAlaiStadium #= D) 
         #\/
         (Skern #= D#/\JaiAlaiStadium #= B) 
         ),

        % 5. Rayou#/\the island that exports bananas are connected by an 
        %    east-west bridge.
        (
         (Rayou #= A#/\Bananas #= B) 
         #\/
         (Rayou #= B#/\Bananas #= A) 
         #\/
         (Rayou #= C#/\Bananas #= D) 
         #\/
         (Rayou #= D#/\Bananas #= C) 
         ),

        % 6. The islands noted for the South Pacific's largest ice skating rink 
        %   #/\for the jai alai stadium are not connected by a bridge.
        ( 
         (IceSkatingRink #= A#/\JaiAlaiStadium #= D)
         #\/
         (IceSkatingRink #= D#/\JaiAlaiStadium #= A)
         
         #\/
         (IceSkatingRink #= B#/\JaiAlaiStadium #= C)
         #\/
         (IceSkatingRink #= C#/\JaiAlaiStadium #= B)
          ),


        % search
        % term_variables([Island,Export,Attraction], Vars),
        append(Island,Export,Vars1),
        append(Vars1,Attraction,Vars),
        labeling([],Vars),

        % print result
        print_all([1,2,3,4], ['A','B','C','D']),
        print_all(Island,IslandS),
        print_all(Export,ExportS),
        print_all(Attraction,AttractionS).


print_all(X,S) :-
        length(X,N),
        (for(I,1,N), param(X,S) do
             nth1(I,X,XI),
             nth1(XI,S,SXI),
             format('<~w>\t', SXI)
        ),nl.

