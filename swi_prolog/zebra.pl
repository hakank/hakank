/*

  Zebra puzzle in SWI Prolog

  Lewis Carrol's classical puzzle with five houses and a zebra:
  
  Five men with different nationalities live in the first five houses
  of a street.  They practise five distinct professions, and each of
  them has a favourite animal and a favourite drink, all of them
  different.  The five houses are painted in different colours.
  
  The Englishman lives in a red house.
  The Spaniard owns a dog.
  The Japanese is a painter.
  The Italian drinks tea.
  The Norwegian lives in the first house on the left.
  The owner of the green house drinks coffee.
  The green house is on the right of the white one.
  The sculptor breeds snails.
  The diplomat lives in the yellow house.
  Milk is drunk in the middle house.
  The Norwegian's house is next to the blue one.
  The violinist drinks fruit juice.
  The fox is in a house next to that of the doctor.
  The horse is in a house next to that of the diplomat.
  
  Who owns a Zebra, and who drinks water?
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   Nat        = [English, Spaniard, Japanese, Italian, Norwegian],
   Color      = [Red, Green, White, Yellow, Blue],
   Profession = [Painter, Sculptor, Diplomat, Violinist, Doctor],
   Pet        = [Dog, Snails, Fox, Horse, Zebra],
   Drink      = [Tea, Coffee, Milk, Juice, Water],

   Nat        ins 1..5,
   Color      ins 1..5,
   Profession ins 1..5,
   Pet        ins 1..5,
   Drink      ins 1..5,

   all_different(Nat),
   all_different(Color),
   all_different(Profession),
   all_different(Pet),
   all_different(Drink),

   English #= Red,
   Spaniard #= Dog,
   Japanese #= Painter,
   Italian #= Tea,
   Norwegian #= 1,
   Green #= Coffee,
   Green #= White + 1,
   Sculptor #= Snails,
   Diplomat #= Yellow,
   Milk #= 3,
   abs(Norwegian - Blue) #= 1,
   Violinist #= Juice,
   abs(Fox-Doctor) #= 1,
   abs(Horse - Diplomat) #= 1,

   flatten([Nat,Color,Profession,Pet,Drink],Vars),
   label(Vars),
   
   NatNames = [English=english, 
               Spaniard=spaniard, 
               Japanese=japanese,
    	       Italian=italian, 
               Norwegian=norwegian],
   member((Zebra=ZebraNat), NatNames),
   member((Water=WaterNat), NatNames),
   format("The ~w owns the zebra\n", ZebraNat),
   format("The ~w drinks water\n", WaterNat).
