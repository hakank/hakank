/* FRèN BYTE APRIL 1988 sid 286 (tillsammans med FUZZU.PRO)
   OBS! En bug i good_movie! p_OR ska komma efter threshold! */

include "FUZZY.PRO"

predicates

scary(symbol)
funny(symbol)
nude(symbol)
good_movie(symbol)
      
clauses
scary(king_kong) :- fuzzy(0.5).
scary(american_werewolf) :- fuzzy(0.5).
scary(alien) :- fuzzy(0.9).
funny(king_kong) :- fuzzy(0.1).
funny(american_werewolf) :- fuzzy(0.5).
funny(alien) :- fuzzy(0.4).
nude(king_kong) :- fuzzy(0.1).
nude(american_werewolf) :- fuzzy(0.1).
nude(alien) :- fuzzy(0.3).

good_movie(X) :-
  init_fuzzy, scary(X), funny(X), nude(X), 
  threshold(0.3), p_OR.

   