/*******  Fr†n BYTE APRIL 1988 sid 285 ff *****************
 Det finns „ven med FUZTEST.PRO och FUZ2TST              */
 
database
   truth(real)
   thresh(real)
   
predicates
   init_fuzzy 
   fuzzy(real)
   threshold(real)
   
   f_OR
   f_AND 
   p_OR 
   p_AND 
   f_NOT
   
   fuzzy_max(real, real, real)
   fuzzy_min(real, real, real)
         
clauses
   init_fuzzy :- asserta(thresh(0.01)).
   
   fuzzy(Truth) :- 
      bound(Truth),
      asserta(truth(Truth)),
      retract(thresh(Thresh)),
      asserta(thresh(Thresh)), !,
      Truth >= Thresh.
      
   fuzzy(Truth) :-
      retract(truth(Truth)),
      asserta(truth(Truth)), !.
      
   threshold(Truth) :-
      bound(Truth),
      retract(thresh(_)),
      asserta(thresh(Truth)),
      retract(truth(Current)),
      asserta(truth(Current)), !,
      Current >= Truth.
      
   threshold(Truth) :-
      retract(thresh(Truth)),
      asserta(thresh(Truth)), !.
      
   f_OR :-
      retract(truth(X)),
      retract(truth(Y)), !,
      fuzzy_max(X, Y, Z), fuzzy(Z), !.
   
   f_AND :-
      retract(truth(X)),
      retract(truth(Y)), !,
      fuzzy_min(X, Y, Z), fuzzy(Z), !.
      
   p_OR :-
      retract(truth(X)),
      retract(truth(Y)), !,
      Z = X + Y - (X * Y), fuzzy(Z), !.
       
   p_AND :-
      retract(truth(X)),
      retract(truth(Y)), !,
      Z = X * Y, fuzzy(Z), !.
      
   f_NOT :-
      retract(truth(X)), !,
      Z = 1 - X, fuzzy(Z), !.
      
   fuzzy_max(X, Y, Z) :- X >= Y, !, Z = X.
   fuzzy_max(_, Y, Z) :- Z = Y.
   
   fuzzy_min(X, Y, Z) :- X <= Y, !, Z = X.
   fuzzy_min(_, Y, Z) :- Z = Y.
            
      
            
        
   
