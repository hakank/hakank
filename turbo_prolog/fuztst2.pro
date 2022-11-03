include "fuzzy.pro"

predicates
  nice_city(symbol)  location(symbol)
  city(symbol)       climate(symbol)
  warm(symbol)       dry(symbol)
  characteristics(symbol)
  population(symbol, integer)
  expensive(symbol)
        
clauses
  nice_city(X) :-
     init_fuzzy, location(X),
     fuzzy(0.5), p_AND,
     characteristics(X),
     fuzzy(0.9), p_AND, p_OR,
     fuzzy(Rating),
     write(X, " has a rating of ", Rating),
     nl.
   
   location(X) :- city(X), climate(X), f_AND.
   
   climate(X)  :- warm(X), dry(X), f_AND.
   
   characteristics(X) :- population(X, Pop),
      Val = Pop/2500.0,
      fuzzy(Val),
      fuzzy(0.9), p_AND,
      expensive(X), f_NOT,
      fuzzy(0.5), p_AND,
      p_OR, f_AND.
      
   city(malm”) :- fuzzy(1.0).
   city(lund) :- fuzzy(1.0).
   city(byn) :- fuzzy(1.0).
   
   warm(malm”) :- fuzzy(0.9).
   warm(lund) :- fuzzy(0.5).
   warm(byn) :- fuzzy(0.9).
   
   dry(malm”) :- fuzzy(0.9).
   dry(lund) :- fuzzy(0.6).
   dry(byn) :- fuzzy(0.7).
   
   population(malm”, 500) :- fuzzy(1.0).
   population(lund, 2000) :- fuzzy(1.0).
   population(byn, 1500) :- fuzzy(1.0).
   
   expensive(malm”) :- fuzzy(0.6).
   expensive(lund) :- fuzzy(0.9).
   expensive(byn) :- fuzzy(0.8).
   
   
   
     
