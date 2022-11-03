/* Lite f”rs”k p† CHAOS-matte 

   Chaos/4 har tv† "moder": Den ena „r matematisk,
   och man f†r ut tal.
   Den andra moden „r grafisk och man f†r ut en r„tt
   fin grafik.
   Det „r bara att kommentera bort den icke-relevanta 
   moden!                                              
  
   Man f†r sj„lv skriva formeln p† den f”rsta raden av 
   funktionen.
   (G†r det att f† s† det g”rs direkt ifr†n start????)
   En intressant sak med formeln: Konstant*A(1-A) 
   „r att talet g†r mot X = (A-1)/A !!!
   F”r tal som „r under 3 ??? 

   Intresanta formler:
   NewA = Konstant * A * tan(A)      (starta med Konstant = ca 0.987)
   NewA = Konstant * A * cos(A)   
*/   

include "GRAPDECL.PRO" 
constants
   bgi_path = ""
   x=0.54321  
predicates

   chaos(real, real, integer, real, real)
   main
   kolla_B(real, real, real, real, real, real)
   skrivut(real, real, real)    
   
clauses 
   
   chaos(A, Konstant, B, F”rsk, Page) :-   
      NewA=Konstant*A*(1-A),                      % FORMELN !!!!!!!!!!!!
/*********** tag bort denna rad
 *****      writef(">>  \t"),
 *****     write("  ", B, "   "),
 *****     write(A, "\n"),
 *****     readchar(Char),
 *****     NewB = B + 1,
 *****     och denna ! **************************/
      PlottA = (NewA)*250,
/*      readchar(Dummy),
      Dummy <> 'q',*/
      NewPage = Page + 1, 
      kolla_B(B, B_check, Konstant, NewKonst, F”rsk, NewPage),
      putpixel(B_check, PlottA, 3),
/*      sound(5, PlottA),*/
      skrivut(NewA, 0, 280),
      Diff_A = abs(NewA - A),
      skrivut(Diff_A, 0, 290),
      Check_diff = Diff_A * NewA,
      skrivut(Check_diff, 170, 290),            
      NewB = B_check + 0.5,
      chaos(NewA, NewKonst, NewB, F”rsk, NewPage).

      /* kolla_b kontrollerar om talet „r st”rre „n 700,
         dvs max f”r sk„rmen. D† s„tts talet till 0. */
   
 kolla_B(Tal, NewTal, Konstant, NewKonst, F”rsk, NewPage)   :-
      Tal > 700, !,
/*      readchar(Dummy),*/
      clearviewport,
      NewKonst = Konstant + F”rsk,
      skrivut(NewKonst, 0, 260),
      Fixad_Page = trunc(NewPage/700)+1,    % F”r att r„kna sidorna
      skrivut(Fixad_Page, 0,270),    
      NewTal = 0.
      
  kolla_B(Tal, Tal, Konstant, Konstant, _, _).       

  
skrivut(Tal, X_koord, Y_koord) :-
      str_real(Tal_Text, Tal),!,
      setcolor(0),
      outtextxy(X_koord, Y_koord,"\8\8\8\8\8\8\8\8\8\8\8\8\8\8\8\8"),
      setcolor(1),
      NewX = X_koord + 5,
      outtextxy(NewX, Y_koord, Tal_Text).

   main :-
     
/*      makewindow(1, 7, 7, "Kaosforskning", 1, 10, 20, 60),*/
      write("Du ska fylla i en konstant och en f”rskjutningsfaktor.\n"),
      write("Konstantens max och min beror p† formeln.\n"),
      write("Om du vill se p† ett l„ngre f”rlopp, "),
      write("s„tt d† \nf”rskjutningen till 0.\n"),
      write("\nSkriv konstanten:   "),
      readreal(Konstant),
      write("\nSkriv f”rskjutningen:  "),
      readreal(F”rsk),
      initgraph(detect, 0, GraphDriver,GraphMode, bgi_path),
      outtextxy(175, 260, "Constant"),
      outtextxy(175, 270, "Page"),
      outtextxy(5, 300, "Deviation"),
      outtextxy(175, 300, "Deviation * Value"),
      chaos(0.5, Konstant, 1, F”rsk, 1).

 goal 
     main