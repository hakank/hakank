/***********   BORING (R) KLARADE DET IGEN!! **************

 Detta program g”r en l„sning av comline (i DOS) och
 tar reda p† vilka parametrar som finns och ger
 r„tt svar p† dem!
 Tv„rr m†ste jag inf”r en andra parameter i kolla-funktionen (Old) 
 som beh†ller den f”rsta comline. Det beror p† rekursivitetens 
 paradoxala unicitet (eller s†!).
 
 (c) BORING Internationel 15 okt 1989
 ****************************************************************/
 
predicates
 main
 kolla(string, string) 
 kolla2(string, string)      

clauses
  
   /* Den f”rsta "kolla" g”r en genomg†ng av parametrarna
      om OLD „r skild fr†n "".
      Den andra tar ”ver om parametern = "" */
  
  kolla(Input, Old) if                        
     fronttoken(Input, Str„ng1, Str„ng2), 
     Old<> "",
     kolla2(Str„ng1, Old), 
     kolla(Str„ng2, Old),!.

  kolla(Input, Old) if 
     Old="",
     write("Du har utl„mnat parametrarna\n"),
     write("Skriv 'COMLINE ?' om du inte vet n†got.\n"). 

   /* kolla2 „r sj„lva parameterlistan.
      Obs! kolla2 har tv† parametrar, varav den sista inte 
      har n†gon berydelse!  */
            
 
   kolla2("hej", _)  if
        write("hejsan sj„lv p† dig", "\n"), !. 
       
    kolla2("?",_) if
        write("Jaha, du vet allts† inte hur det funkar!?\n"), 
        write("'COMLINE' „r det nya programmet fr†n BORING\n"),
        write("Du har f”ljande val:\n"),
        write("COMLINE (text)\n"),
        write("D„r text st†r f”r:\n"),
        write("HEJ, eller KALLE eller HOPPSAN\n"),!.
    
    kolla2("kalle", _) if
        write("Vad har kalle med detta att g”ra?\n"), !.

    kolla2(_,_)  if 
         write("Fel parameter!"),fail.    
   
    main if 
      comline(Inp1),
      upper_lower(Inp1, X),
      kolla(X, X).

goal
    main.
    

   