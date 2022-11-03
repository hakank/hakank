%test p† database 9005??
database - personDB

likes(string, string)
dislikes(string, string)
person(string, string, integer)

predicates
main

clauses
person(fred, capitola,23).
person(nisse, karlsson, 31).
person(olle, svensson, 29).
person(ulla, nilsson, 17).

likes(fred, pengar).
likes(olle, pengar).
likes(ulla, nisse).
likes(nisse, ulla).

dislikes(ulla, pengar).
dislikes(olle, nisse).


main if
 consult("myfirst.dba", personDB),
 write("Skriv in efternamn:\n"),
 readln(Efternamn),
 write("Skriv in f”rnamn:\n"),
 readln(F”rnamn),
 write("Skriv in †lder:\n"),
 readint(lder),
 assertz(person(F”rnamn, Efternamn, lder), personDB),
 retract(person(F”r_namn, Efter_namn, Age), personDB),
 write(F”r_namn, " ", Efter_namn," ", Age, "\n"),
 fail.
    

   