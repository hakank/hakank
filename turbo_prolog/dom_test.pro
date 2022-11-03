% F”rs”k till domain-"diskussioner" 90-02-19
% jag har i ett par veckor prog i C!
% vidare. f”rs”k till databasprogrammering (intern)
% problem: ibland tas de personen som inte (obs!) „r assertade!!!
% med, ibland inte. har jag trace p† tas de alltid med!!!
% aha, det har s„kert med en reset av db-pekaren att g”ra?!
% vilket sker om man kompilerar om!!!!!!!!
domains
    name   = name(efternamn, f”rnamn)
    adress = adress(gata, postnr, stad) 
    f”rnamn, efternamn, gata, stad = symbol
    postnr, telefon = real

    
database  -  mydatabase
    person(name, adress, telefon)

predicates
   main1
   main2
   main3
   main4
   main5
   man(symbol, symbol)
  grannar(efternamn, efternamn, gata)    
   grannar(efternamn, efternamn, efternamn, gata)    
   s„tt_in
clauses
man(Namn, Adress):-
	person(name(Namn,_), adress(Adress,_,_), _).


grannar(Namn1, Namn2, Gata) :-
        retract(person(name(Namn1, _), adress(Gata, _, _), _ )),
        retract(person(name(Namn2, _), adress(Gata, _,_ ), _ )), 
        Namn1 <> Namn2.

grannar(Namn1, Namn2, Namn3, Gata) :-
        retract(person(name(Namn1, _), adress(Gata, _, _), _ )),
        retract(person(name(Namn2, _), adress(Gata, _,_ ), _ )), 
        retract(person(name(Namn3, _), adress(Gata, _,_ ), _ )), 
        Namn1 <> Namn2,!, 
        Namn2 <> Namn3.


        
% dessa tas inte alltid med !!!!!!!
% det funkar om man skriver assertz i st f asserta
% f”rst†r inte riktigt kopplingen h„r!!!?

s„tt_in :-
asserta(person(name(kahrlsson, nisse), adress(ekgatan_25, 21232, malm”), 133333)),
asserta(person(name(karlzzon, kalle), adress(ekgatan_25, 21234, malm”), 123234)),
assertz(person(name(ohlsson, kalle), adress(ekgatan_26, 21234, malm”), 123234)),
assertz(person(name(ohlzzon, kalle), adress(ekgatan_26, 21234, malm”), 123234)),
assertz(person(name(olzohn, kalle), adress(ekgatan_26, 21234, malm”), 123234)).
      
main1 :-
   	write("Databas f”r m„n:\n"),
 	person(name(Efternamn, F”rnamn), adress(Gata,_, Stad), Telefon_nr),
	write(F”rnamn, "  ", Efternamn, "\n", Gata, " ", Stad,   "\n", Telefon_nr, "\n"),
        fail.   
 main2 :-
	man(X, Y), write("\n", X, " ", Y, "\n"), fail.
 
  main3 :-       % nu med databas
	 man(Namn, Gata),
	 retract( person ( name ( Efternamn, F”rnamn ), adress ( Gata, _, _) , _) ),
	 write("\n", F”rnamn, " ", Efternamn, " ", Gata),
	 fail.

  main4 :-    % det „r denna som  „r den korekta
  	assertz(person(name(svensson, olle), adress(brukg_10, 21221, malm”), 21221)),
  	assertz(person(name(svenzzon, kalle), adress(brukg_10, 23221, karlstad), 21221)),
 % obs inte retract h„r: det g”rs i grannar-modulen!!
 %  	retract( person ( name ( Efternamn, F”rnamn ), adress ( Gata, _, _) , _) ),
	write("\nP† samma gata bor:\n"),
	grannar(E, N, G),
	write(E, " ", N, " ", "\n", G, "\n"),
	grannar(Efternamn, Namn2, Namn3, Gata),
	write(Efternamn, " ", Namn2, " ", Namn3,  "\n", Gata, "\n"),
	fail.

main5 :-    % test p† grannar
        grannar(X, Y, Z , P),
        write(X, "  ", Y, " ", Z, "\n").   
      
goal
   s„tt_in,         
   main4        
   