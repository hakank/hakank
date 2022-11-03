/*               Mitt f”rsta databasprogram
OBS Att man m†ste retractall innan man b”rja p† en ny modul,
annars h„mtas databasen dubbelt (eller tripelt etc)          */

database - personDB

person(string, string, integer)

predicates
 main
 tagbort(string, string)
 lista(string)

clauses

lista(E_namn) if
  retractall(person(_,_,_), personDB),
  consult("myfirst.dba", personDB),
  retract(person(_, E_namn, _), personDB),
  write(E_namn,"\n"), fail.  
 
tagbort(Namn, Kvar) if
  retractall(person(_,_,_), personDB),
  consult("myfirst.dba", personDB),
  retract(person(_, Namn, _)), 
  save("myfirst.dba", personDB),
  lista(Kvar). 
  
main if
 write("Skriv in efternamn:\n"),
 readln(Efternamn),
 write("Skriv in f”rnamn:\n"),
 readln(F”rnamn),
 write("Skriv in †lder:\n\n"),
 readint(lder),
 consult("myfirst.dba", personDB),
 assertz(person(F”rnamn, Efternamn, lder), personDB),
 save("myfirst.dba", personDB),
 retract(person(F”r_namn, Efter_namn, Age), personDB),
 write(Efter_namn, " ", F”r_namn," ", Age, "\n"),
 fail.
    

   