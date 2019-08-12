/*

  Exodus puzzle (Dell Logic Puzzles) in SWI Prolog

  From http://brownbuffalo.sourceforge.net/ExodusClues.html
  """
  Title: Exodus
  Author: Sophy McHannot
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 14
  Stars: 2

  In preparation for Passover, five children at Hebrew school 
  (Bernice,Carl,Debby,Sammy, and Ted) 
  have been chosen to present
  different parts of the story of the Exodus from Egypt 
   (burning bush, captivity,
    Moses's youth, Passover, or the Ten Commandments). 
  Each child is a different age 
    (three, five, seven, eight, or ten), 
  and the family of each child has recently made its own exodus 
  to America from a different country 
  (Ethiopia, Kazakhstan, Lithuania, Morocco, or Yemen). 
  Can you find the age of each child, his or her family's country of 
  origin, and the part of the Exodus story each related?

   1. Debby's family is from Lithuania.
   2. The child who told the story of the Passover is two years older
      than Bernice.
   3. The child whose family is from Yemen is younger than the child from
      the Ethiopian family.
   4. The child from the Moroccan family is three years older than Ted.
   5. Sammy is three years older than the child who told the story of
      Moses's youth in the house of the Pharaoh.
   6. Carl related the story of the captivity of the Israelites in Egypt.
   7. The five-year-old child told the story of the Ten Commandments.
   8. The child who told the story of the burning bush is either two or
      three years older than the one whose family came from
      Kazakhstan.

  Determine: Age -- Child -- Country -- Story
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   N = 5,

   Bernice = 1,
   Carl    = 2,
   Debby   = 3,
   Sammy   = 4, 
   Ted     = 5,
   Children = [Bernice, Carl, Debby, Sammy, Ted],
   ChildrenS = ["Bernice", "Carl", "Debby", "Sammy", "Ted"],

   Story = [BurningBush, Captivity, MosessYouth, Passover,TenCommandments],
   Story ins 1..N,
   StoryS = ["Burning Bush", "Captivity", "MosessYouth", "Passover","Ten Commandments"],

   length(Age,N),
   Age ins 3\/5\/7\/8\/10,

   Country = [Ethiopia, Kazakhstan, Lithuania, Morocco, Yemen],
   Country ins 1..N,
   CountryS = ["Ethiopia", "Kazakhstan", "Lithuania", "Morocco", "Yemen"],

   all_different(Story),
   all_different(Age),
   all_different(Country),

   %% constraints 
   Debby #= Lithuania,

   element(Ted,Age,AgeTed),
   element(Ethiopia,Age,AgeEthiopia),
   element(Passover,Age,AgePassover),
   element(Bernice,Age,AgeBernice),
   element(Yemen,Age,AgeYemen),
   element(Morocco,Age,AgeMorocco),
   element(Sammy,Age,AgeSammy),
   element(MosessYouth,Age,AgeMosessYouth),
   element(TenCommandments,Age,AgeTenCommandments),
   element(BurningBush,Age,AgeBurningBush),
   element(Kazakhstan,Age,AgeKazakhstan),

   AgePassover #= AgeBernice + 2,
   AgeYemen #< AgeEthiopia,
   AgeMorocco #= AgeTed + 3,
   AgeSammy #= AgeMosessYouth + 3,
   Carl #= Captivity,
   AgeTenCommandments #= 5,
   ( 
       (AgeBurningBush #= AgeKazakhstan + 2)
       #\/
       (AgeBurningBush #= AgeKazakhstan + 3)
   ),

   %% search
   flatten([Story,Age,Country],Vars),
   labeling([],Vars),

   %% print solution
   writeln(children=Children),
   writeln(story=Story),
   writeln(country=Country),
   writeln(age=Age),
   nl,
   
   pretty_print(Children,ChildrenS),
   pretty_print(Story,StoryS),
   pretty_print(Country,CountryS),
   pretty_print([1,2,3,4,5],Age),

   nl.

%%
%% Pretty print solution
%%
pretty_print(X,S) :-
        inverse(X,Inv),
        length(Inv,Len),
        findall(E,
                (
                 between(1,Len,I),
                 nth1(J,Inv,I),
                 nth1(J,S,E)
                ),
                Sol),
        format("~t~w~12|~t~w~23|~t~w~37|~t~w~47|~t~w~64|~n",Sol).

