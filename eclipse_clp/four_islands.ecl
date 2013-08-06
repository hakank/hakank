/*

  Four Islands puzzle (Dell Logic Puzzles) in ECLiPSe.


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

  And also the F1 model
  http://www.f1compiler.com/samples/Four%20Islands.f1.html



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
:-lib(propia).


go :-
        N = 4,
        Range = 1..N,

        A = 1,
        B = 2,
        C = 3,
        D = 4,

        Island = [](Pwana, Quero, Rayou, Skern),
        Island :: Range,
        IslandS = []("Pwana", "Quero", "Rayou", "Skern"),

        Export = [](Alabaster, Bananas, _Coconuts, DurianFruit),
        Export :: Range,
        ExportS = []("alabaster", "bananas", "coconuts", "durian_fruit"),
        
        Attraction = [](ResortHotel, IceSkatingRink, JaiAlaiStadium,
                        KoalaPreserve),
        Attraction :: Range,
        AttractionS = []("resort_hotel", "ice_skating_rink",
                         "jai_alai_stadium", "koala_preserve"),

        
        alldifferent(Island),
        alldifferent(Export),
        alldifferent(Attraction),


        % 1. The island noted for its koala preserve is due south of Pwana.
        (
         (Pwana #= A and KoalaPreserve #= C)
         or
         (Pwana #= B and KoalaPreserve #= D)
         ),

        % 2. The island with the largest alabaster quarry is due west of Quero.
        ( 
         (Alabaster #= A and Quero #= B) 
         or 
         (Alabaster #= C and Quero #= D) 
          ),

        % 3. The island with the resort hotel is due east of the one 
        %    that exports durian fruit.
        ( 
         (DurianFruit #= A and ResortHotel #=  B )
         or
         ( DurianFruit #= C and ResortHotel #=  D)
          ),

        % 4. Skern and the island with the jai alai stadium are connected by a 
        %    north-south bridge. 
        (
         (Skern #= A and JaiAlaiStadium #= C) 
         or
         (Skern #= C and JaiAlaiStadium #= A) 
         or
         (Skern #= B and JaiAlaiStadium #= D) 
         or
         (Skern #= D and JaiAlaiStadium #= B) 
         ),

        % 5. Rayou and the island that exports bananas are connected by an 
        %    east-west bridge.
        (
         (Rayou #= A and Bananas #= B) 
         or
         (Rayou #= B and Bananas #= A) 
         or
         (Rayou #= C and Bananas #= D) 
         or
         (Rayou #= D and Bananas #= C) 
         ),

        % 6. The islands noted for the South Pacific's largest ice skating rink 
        %    and for the jai alai stadium are not connected by a bridge.
        ( 
         (IceSkatingRink #= A and JaiAlaiStadium #= D)
         or
         (IceSkatingRink #= D and JaiAlaiStadium #= A)
         
         or
         (IceSkatingRink #= B and JaiAlaiStadium #= C)
         or
         (IceSkatingRink #= C and JaiAlaiStadium #= B)
          ),


        % search
        term_variables([Island,Export,Attraction], Vars),
        labeling(Vars),

        % print result
        calc_size([](IslandS,ExportS,AttractionS), Size),
        print_all([](1,2,3,4), []("A","B","C","D"), Size),
        print_all(Island,IslandS,Size),
        print_all(Export,ExportS,Size),
        print_all(Attraction,AttractionS,Size).


print_all(X,S,Size) :-
        dim(X,[N]),
        concat_string(["%",Size, "s"],Format),
        (for(I,1,N), param(X,S,Format) do
             This is S[X[I]],
             printf(Format, [This])
        ),nl.


%
% Size is 2 + the length of the largest string.
% Used for the presentation of the result.
% 
calc_size(A, Size) :-
        flatten_array(A,List),
        ( foreach(String,List),
          foreach(Len, Lengths) do
              string_length(String,Len)
        ),
        Size is maxlist(Lengths) + 2.

        