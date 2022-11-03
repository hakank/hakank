config "pie.sys"                   

predicates
   start
   ide(string)
   plott(string)
   karakt„r(string, string, string)
   main
   shift(integer)      
   clear
   ihop(string, string)      
   tabort(integer, integer)
clauses
   
   shift(F”nster) if
      shiftwindow(F”nster).
   
   clear if
      clearwindow.
   start  if
      shift(1),
      write("Detta „r en novelizator fr†n BORING Inc (R), Int   (C) 1989.\n"),
      write("Du fyller bara i ett antal uppgifter. OBS MAX 255 tecken!\n"),
      write("Tryck p† ENTER n„r du l„st detta."), readln(X),
      clear.
      
   ide(IDE) if
      shift(1),
      write("Skriv id‚n:\n"),
      readln(IDE),
      shift(2),
      write(IDE). 
      
    plott(PLOT) if
      shift(1),
      clearwindow,
      write("Skriv plotten\n"),
      readln(PLOT),
      shift(3),
      write(PLOT).
       
   karakt„r(KARA1, KARA2, KARA3) if
      shift(1),
      clearwindow,
      write("Skriv 1:a karakt„ren\n"),readln(KARA1),
      shift(4), write(KARA1),
      shift(1), clear, write("2:a karakt„ren\n"), readln(KARA2),
      shift(5), write(KARA2),
      shift(1), clear, write("3:e karakt„ren\n"), readln(KARA3),
      shift(6), write(KARA3).
   
   ihop(STRŽNG1, STRŽNG2) if
      concat(STRŽNG1, "\n", STRŽNG2).
    
   tabort(WindowNo, X) if       
      removewindow(WindowNo, X).
      
   main if
      makewindow(1, 7, 15, " *** BORING'S Novelizator, version 1.0 *** ", 0, 5, 5, 70),
      makewindow(2, 7, 15, "  Id‚f”nster  ", 5, 5, 4, 70),
      makewindow(3, 7, 15, "  Plottf”nster", 9, 5, 4, 70),
      makewindow(4, 7, 15, "  Karakt„r 1  ", 13, 5, 4, 70),
      makewindow(5, 7, 15, "  Karakt„r 2  ", 17, 5, 4, 70),
      makewindow(6, 7, 15, "  Karakt„r 3  ", 21, 5, 4, 70),
      start,
      ide(IDE), 
      plott(PLOT),
      karakt„r(KARA1, KARA2, KARA3),
      shift(1),
      clearwindow,
      tabort(1, 1),tabort(2, 1), tabort(3, 1), tabort(4, 1),
      tabort(5, 1), tabort(6, 1),  
      makewindow(7, 7, 15, "EDITF™NSTER", 0, 0, 25, 80), 
      ihop(IDE, IDE1), ihop(PLOT, PLOT1), ihop(KARA1, KARA11),
      ihop(KARA2, KARA21), ihop(KARA3, KARA31),   
      concat(IDE1,   PLOT1, Text1),
      concat(Text1, KARA11, Text2),
      concat(Text2, KARA21, Text3),
      concat(Text3, KARA31, Text4),
      edit(Text4, Text).
        
goal
      main.