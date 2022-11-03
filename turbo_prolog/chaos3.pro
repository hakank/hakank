% Lite f”rs”k p† CHAOS-matte 
%   Chaos3 
% Plotta f”rst n„r formeln "stadgat sig"
% och att finna attractorn
include "GRAPDECL.PRO" 

constants
   bgi_path = ""
   x=0.54321  
predicates

   chaos(real, real, integer, real)
%   chaos(real, real, integer, real, integer, integer)
    main
   starta_grafik()
         
clauses 
   

chaos(A, Konstant, B, F”rsk) :-   
      B < 100, 
      Konstant < 4.001, !,
      NewA = Konstant * A * (1-A),                      % FORMELN !!!!!!!!!!!!
      PlottA = NewA * 250,
/*      readchar(Dummy),
      Dummy <> 'q',*/
      NewB = B + 1.0,
      chaos(NewA, Konstant, NewB, F”rsk).

chaos(A, Konstant, B, F”rsk) :-   
      B > 99, 
      Konstant < 4.001, !,
      NewA = Konstant*A*(1-A),                      % FORMELN !!!!!!!!!!!!
      PlottA = NewA * 250,
/*      readchar(Dummy),
      Dummy <> 'q',*/
      write("Konstant = ", Konstant, "\nA = ", A, "\n"),
%      putpixel(B, A, 3),
      NewB = 1,
      NewKonst = Konstant + F”rsk,
      chaos(x, NewKonst, NewB, F”rsk).

chaos(A, Konstant, B, F”rsk) :-   
   Konstant <= 4.001,
   write("S† var det klart!").
    
starta_grafik :-
initgraph(detect, 0, GraphDriver,GraphMode, bgi_path).

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
%      starta_grafik,
      chaos(x, Konstant, 1, F”rsk).

 goal 
     main