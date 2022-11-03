/* Lite f”rs”k p† CHAOS-matte */

include "GRAPDECL.PRO" 
constants
   bgi_path = ""
   x=0.54321
   
predicates

   chaos(real, real, integer)
   main
   kolla_B(real, real)

clauses 

/* Detta „r den matematiska varianten
            Formel: X**2 - 1               */
            
/*   chaos(A, Konstant, Count) :-
      NewA=Konstant*A*A-1,
      writef(">>  \t"),
      write("  ", Count, "   "),
      PlottA = NewA + 1,
      write(PlottA, "\n"),
      putpixel(A, 50, 3),
      readchar(Char),
      NewCount=Count+1,
      chaos(NewA, Konstant, NewCount).       */

/* Nedanst†ende „r den grafiska varianten!  */

   chaos(A, Konstant, B) :-   
      NewA=(Konstant*A*A)-1,
      PlottA = (NewA + 1)*150 ,
      kolla_B(B, B_check),
      putpixel(B_check, PlottA, 3),
      NewB = B_check + 0.5,
      chaos(NewA, Konstant, NewB).
   
   kolla_B(Tal, NewTal)   :-
      Tal > 700, !,
/*      readchar(Dummy),*/
      clearviewport,
      NewTal = 1.
      
   kolla_B(Tal, Tal).       
   
   main :-
/*      write("Skriv X: (mellan 0 - 1)   "),
      readreal(X), */
      write("\nSkriv konstanten: (mellan 0 - 2)    "),
      readreal(Konstant),
      initgraph(detect, 0, GraphDriver,GraphMode, bgi_path),
      chaos(x, Konstant, 1).
   
 goal 
     main