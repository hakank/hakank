/******** OBS  funkar inte helt bra! ***********/





predicates

load(string)
main      
clauses

    

load(Input)    if
      system(Input).

main if
    write("Detta „r BORINGS skal-system\n"),
    readln(X),
    system("dir/p *.exe"), 
    write("Skriv vilken fil du vill h„mta\n"),
    readln(Input),
    load(Input),
    write("Jaha").
    
goal
   main.
   



   