/*

  Tunapalooza puzzle (Dell Logic Puzzles) in SICStus Prolog.

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
  c) The two friends won't meet at the T-shirt vendor while Yellow Reef 
     is playing.
  5. The country and speed metal acts are, in some order, Retread Ed 
     and the Flat Tires and the act during which Tim and Kerri will 
     meet at the mosh pit.
  6. The reggae band is neither Korrupt nor the act during which Tim and 
     Kerri will meet at the information booth.
  
  Determine: Band name -- Music type -- Meeting place
  """

  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/tunapalooza.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/tunapalooza.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 4,

        Ellyfish                      = 1,
        Korrupt                       = 2,
        Retread_Ed_and_the_Flat_Tires = 3,
        Yellow_Reef                   = 4,
        RockBand = [Ellyfish,Korrupt,Retread_Ed_and_the_Flat_Tires,Yellow_Reef],
        
        Genre = [Country, Grunge, Reggae, SpeedMetal],
        domain(Genre,1,N),

        Rendevouz = [CarnivalGames, InformationBooth, MoshPit, TShirtVendor],
        domain(Rendevouz,1,N),

        all_distinct(Genre),
        all_distinct(Rendevouz),

        % 1. Korrupt isn't a country or grunge music band.
        (Korrupt #\= Country #/\ Korrupt #\= Grunge),

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
        R1 in 0..1,
        R2 in 0..1,
        R3 in 0..1,
        Ellyfish #= Grunge #<=> R1 #= 1,
        InformationBooth #\= Retread_Ed_and_the_Flat_Tires #<=> R2 #= 1,
        TShirtVendor #\= Yellow_Reef #<=> R3 #= 1,
        R1 + R2 + R3 #= 2,
        
        % 5. The country and speed metal acts are, in some order, Retread Ed 
        %    and the Flat Tires and the act during which Tim and Kerri will 
        %    meet at the mosh pit.
        (  
         ( Country #= Retread_Ed_and_the_Flat_Tires #/\ SpeedMetal #= MoshPit )
        #\/
         ( SpeedMetal #= Retread_Ed_and_the_Flat_Tires #/\ Country #= MoshPit )
        ),
        
       
        % 6. The reggae band is neither Korrupt nor the act during
        %    which Tim and Kerri will meet at the information booth.
        Reggae #\= Korrupt,
        Reggae #\= InformationBooth,

        append(Genre,Rendevouz, Vars),

        labeling([],Vars),
        write('Rockband ':RockBand),nl,
        write('Genre    ':Genre),nl,
        write('Rendevouz':Rendevouz),nl,nl,fail.