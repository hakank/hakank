/*
  
  Tunalalooza puzzle (Dell Logic Puzzles) in ECLiPSe.
  
  http://brownbuffalo.sourceforge.net/TunapaloozaClues.html
  """
  Title: Tunapalooza
  Author: Eliot George
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 10
  Stars: 2
 
  Tim and Keri have a full day ahead for themselves as they plan to see 
  and hear everything at Tunapalooza '98, the annual save-the-tuna benefit 
  concert in their hometown. To cover the most ground, they will have to 
  split up. They have arranged to meet during four rock band acts 
  (Ellyfish, Korrupt, Retread Ed and the Flat Tires, and Yellow Reef) at 
  planned rendezvous points (carnival games, information booth, mosh pit, 
  or T-shirt vendor). Can you help match each band name with the type of 
  music they play (country, grunge, reggae, or speed metal) and Tim and 
  Kerri's prearranged meeting spot while they play?
  
  1. Korrupt isn't a country or grunge music band.
  2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
     performance.
  3. The pair won't meet at the T-shirt vendor during the reggae band's show.
  4. Exactly two of the following three statements are true:
  a) Ellyfish plays grunge music.
  b) Tim and Kerri won't meet at the information booth during a 
     performance by Retread Ed and the Flat Tires.
  c) The two friends won't meet at the T-shirt vendor while Yellow Reef is playing.
  5. The country and speed metal acts are, in some order, Retread Ed 
     and the Flat Tires and the act during which Tim and Kerri will 
     meet at the mosh pit.
  6. The reggae band is neither Korrupt nor the act during which Tim and 
     Kerri will meet at the information booth.
  
  Determine: Band name -- Music type -- Meeting place
  """


  Also see the MiniZinc model 
  http://www.hakank.org/minizinc/tunapalooza.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).

go :-
        N = 4,
        R = 1..N,

        Ellyfish = 1,
        Korrupt = 2,
        Retread_Ed_and_the_Flat_Tires = 3,
        Yellow_Reef =4,
        RockBand = [Ellyfish,Korrupt,Retread_Ed_and_the_Flat_Tires,Yellow_Reef],
        
        Genre = [Country, Grunge, Reggae, SpeedMetal],
        Genre :: R,

        Rendevouz = [CarnivalGames, InformationBooth, MoshPit, TShirtVendor],
        Rendevouz :: R,

        alldifferent(Genre),
        alldifferent(Rendevouz),

        % 1. Korrupt isn't a country or grunge music band.
        (Korrupt #\= Country and Korrupt #\= Grunge),

        % 2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
        %    performance.
        Ellyfish #\= CarnivalGames,

        % 3. The pair won't meet at the T-shirt vendor during the reggae 
        %    band's show.
        Reggae #\= TShirtVendor,

        % 4. Exactly two of the following three statements are true:
        % a) Ellyfish plays grunge music.
        % b) Tim and Kerri won't meet at the information booth during a 
        %    performance by Retread Ed and the Flat Tires.
        % c) The two friends won't meet at the T-shirt vendor while 
        %    Yellow Reef is playing.
        (
            (Ellyfish #= Grunge) 
        +
        (InformationBooth #\= Retread_Ed_and_the_Flat_Tires)
        + 
        (TShirtVendor #\= Yellow_Reef)
        ) #= 2,
        
        % 5. The country and speed metal acts are, in some order, Retread Ed 
        %    and the Flat Tires and the act during which Tim and Kerri will 
        %    meet at the mosh pit.
        (  
            ( Country #= Retread_Ed_and_the_Flat_Tires and SpeedMetal #= MoshPit )
        or
        ( SpeedMetal #= Retread_Ed_and_the_Flat_Tires and Country #= MoshPit )
        ),
        
       
        % 6. The reggae band is neither Korrupt nor the act during which Tim and 
        %    Kerri will meet at the information booth.
        Reggae #\= Korrupt,
        Reggae #\= InformationBooth,

        term_variables([Genre,Rendevouz], Vars),

        % search(Vars,0, first_fail, indomain_min, complete, [node(daVinci)]),
        labeling(Vars),
        writeln("Rockband ":RockBand),
        writeln("Genre    ":Genre),
        writeln("Rendevouz":Rendevouz),nl,
        fail.