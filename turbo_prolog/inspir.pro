/****************   BORINGS INSPIRATIONSPROGRAM ***************
  Du kan skriva en text och n„r du vill f†r ett litet inspirations-
  meddelande, genom att trycka ESC, eller flera g†nger.
  
  Du slutar genom att skriva ett litet 'q' i inpirations
  f”nstret.

 Jag „r lite os„ker om stacken eller heapen kommer att h†lla
 f”r lite l„ngre texter. 
 
 Boring Inc Int (R)        (C) 17 oktober 1989
 *************************************************************/

config "prolog.sys"


predicates

     main
     skriv(string, string, integer, integer)
     inspir(string, string, integer, integer) 
     slump_med(string)
     meddelande(integer, string)

clauses
   
   inspir(Text, Text, Row, Column) if
      shiftwindow(1),  /* h„r byter vi f”nster s† att ESC kan verka!!!*/
      slump_med(X),
      write("\n"),  
      time(H, M, S, U),
      write("Klockan „r: ", H, ".", M, "\n", X, "\n"),
      readchar(Quit),
      Quit<>'q',
      shiftwindow(2),
      skriv(Text, Text, Row, Column).
  
    skriv(Text, Text, Row, Column) if
      cursor(Row, Column), edit(Text, Text1),       
      inspir(Text1, Text1, Row, Column).

   slump_med(SLUMP) if
      random(5, X),
      meddelande(X, SLUMP).
      
   main if
      system("CLS"),
      makewindow(1, 7, 15, "Inspirationsf”nster", 0, 5, 5, 70),
      makewindow(2, 7, 15, "** Skrivf”nster ** ESC ger inspiration // lilla 'q' f”r avsluta ", 5, 0, 19, 80),
      shiftwindow(1), 
      write("Du f†r inspiration genom att trycka ESC eller en flera g†nger.\n"),
      write("Avsluta  genom att skriva ett litet 'q' i inspirationsf”nstret.\n"),
      write("Tryck p† Enter f”r att g† vidare!"),
      readln(X), clearwindow,
      shiftwindow(2),
      edit("", Text),
      skriv(Text, Text, _,_).

meddelande(0, "Inga goda ting „r noll") if !.
meddelande(1, "Alla goda ting „r ett") if !.
meddelande(2, "Alla goda ting „r tv†") if !.
meddelande(3, "Alla goda ting „r tre") if !.
meddelande(4, "Alla goda ting „r fyra") if !.
meddelande(5, "Alla goda ting „r fem") if !.
      

goal
   main.




   