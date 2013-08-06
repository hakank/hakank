/*

  Breaking news puzzle (Dell Logic Puzzles) in SICStus Prolog.

  From http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
  """
  Title: Breaking News
  Author: Faith Johnson
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1

  The Daily Galaxy sent its four best reporters 
      (Corey, Jimmy, Lois, and Perry) 
  to different locations 
      (Bayonne, New Hope, Port Charles, and South Amboy) 
  to cover four breaking news events 
      (30-pound baby, blimp launching, skyscraper dedication, and 
       beached whale). 
  Their editor is trying to remember where each of the reporters is. 
  Can you match the name of each reporter with the place he or she 
  was sent, and the event that each covered?

  1. The 30-pound baby wasn't born in South Amboy or New Hope.
  2. Jimmy didn't go to Port Charles.
  3. The blimp launching and the skyscraper dedication were covered, 
     in some order, by Lois and the reporter who was sent to Port Charles.
  4. South Amboy was not the site of either the beached whale or the 
     skyscraper dedication.
  5. Bayonne is either the place that Corey went or the place where 
     the whale was beached, or both.

  Determine: Reporter -- Location -- Story
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/breaking_news.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/breaking_news.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 4,

        Corey = 1,
        Jimmy = 2,
        Lois  = 3,
        Perry = 4,
        Reporters = [Corey, Jimmy, Lois, Perry],
        ReportersS = ['Corey', 'Jimmy', 'Lois', 'Perry'],

        Locations = [Bayonne, NewHope, PortCharles, SouthAmboy],
        LocationsS = ['Bayonne', 'New Hope', 'Port Charles', 'South Amboy'],
        domain(Locations,1,N),
        
        News = [Baby, Blimp, Skyscraper, Whale],
        NewsS = ['Baby', 'Blimp', 'Skyscraper', 'Whale'],
        domain(News,1,N),
        
        all_different(Locations),
        all_different(News),
        
        % 1. The 30-pound baby wasn't born in South Amboy or New Hope.
        Baby #\= SouthAmboy,
        Baby #\= NewHope,
        
        % 2. Jimmy didn't go to Port Charles.
        Jimmy #\= PortCharles,
        
        % 3. The blimp launching and the skyscraper dedication were covered, 
        %    in some order, by Lois and the reporter who was sent to 
        %    Port Charles.
        Lois #\= PortCharles,
        ( 
            (Blimp #= Lois #/\ Skyscraper #= PortCharles)
        #\/
            (Skyscraper #= Lois #/\ Blimp #= PortCharles)
        ),

        % 4. South Amboy was not the site of either the beached whale or the 
        %    skyscraper dedication.
        SouthAmboy #\= Whale,
        SouthAmboy #\= Skyscraper,

        % 5. Bayonne is either the place that Corey went or the place where 
        %    the whale was beached, or both.
        ( 
            Bayonne #= Corey #\/ Bayonne #= Whale
        ),

        append(Locations,News, Vars),
        labeling([],Vars),

        print_all(Reporters,ReportersS),
        print_all(Locations,LocationsS),
        print_all(News,NewsS),
        nl.
        



print_all(X,S) :-
        length(X,Len),
        (for(I,1,Len), 
         param(X,S) do            
             nth1(IX,X,I),
             nth1(IX,S,This),
             format('<~w>\t', [This])
        ),nl.

