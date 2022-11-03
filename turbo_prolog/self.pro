predicates
   kolla(string)
   main
      
clauses
   
   kolla("?") if
       write("Detta „r programmet d„r du kan:\n"),
       write("1. Tycka om dina med m„nniskor.\n"),
       write("2. St† p† huvudet utan att tappa hum”ret.\n"),
       write("3. Klia dig p† armen.\n"),
       write("4. Undra vad du ska g”ra h„rn„st.\n\n"), 	  
       write("Skriv SELF (siffra) f”r att k”ra programmet."),
       !,fail.
   
   kolla("1") if
       write("Tyck om dina med m„nniskor!\n\n"),!.
       
   kolla("2") if
       write("St† p† huvudet men tappa inte hum”ret.\n\n"),!.
       
   kolla("3") if
       write("Klia dig p† armen.\n\n"),!.

   kolla("4") if
       write("Undra nu vad du ska g”ra.\n\n"), !.
   kolla(_) if 
       write("Skriv 'SELF ?' om du inte vet vad du ska g”ra!\n"),
       fail,!.
   
   main if
      comline(Input),
      system("CLS"),
      write("***** B O R I N G (R) International, 1989 *****\n"),
      write("V„lkommen till B O R I N G -- S E L F  (c) 1989 \n\n"), 
      kolla(Input),
      write("Det var v„l inte s† sv†rt!").
      
goal
  main.
   