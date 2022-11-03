% Ett mycket primitivt expertsystem,
% Helt utan smackigt interface

predicates
sympt(string)
symptom(string, string, string, string, string)
main

clauses

sympt(S) if
   symptom(_,_,_,_,S). 

symptom(y,y,y,y, "Woody Allen") if !.
symptom(y,y,n,y, "Groucho Marx") if !.
symptom(y,n,y,y, "Mel Brooks") if !.
symptom(y,n,n,y, "Chaplin") if !.
symptom(n,y,y,y, "H†kan") if !.
symptom(n,y,n,y, "El Truco") if!.
symptom(n,n,y,y, "Raskenstam") if !.
symptom(n,n,n,y, "Pelle Svarsl”s") if!.

symptom(y,y,y,n, "Karl Perkins") if !.
symptom(y,y,n,n, "Groucho Marx2") if !.
symptom(y,n,y,n, "Mel Brooks2") if !.
symptom(y,n,n,n, "Chaplin2") if !.
symptom(n,y,y,n, "H†kan2") if !.
symptom(n,y,n,n, "El Truco2") if!.
symptom(n,n,y,n, "Raskenstam2") if !.
symptom(n,n,n,n, "Pelle Svarsl”s2") if!.

symptom(_,_,_,_,_) if 
       write("Det st„mmer inte!\nTryck p† SPACE!\n"),
       readchar(X), !, main.

main if
    makewindow(1, 7, 7, "Expertsystem", 1, 10, 24, 52),
    write("Besvara fr†gorna efter hand med 'y' eller 'n'!\n"),
    write("Anv„nder han snusk?\n"),
    readln(Svar1),
    write("Anv„nder han vitsar ?\n"),
    readln(Svar2),
    write("Anv„nder han kroppshumor?\n"),
    readln(Svar3),
    write("Anv„nder han veneriska gester?\n"),
    readln(Svar4),
    symptom(Svar1, Svar2, Svar3, Svar4, Result), 
    write("\nSvaret „r ",Result,"\n").


goal
   main.