predicates
test(integer, integer)

      
clauses

   

   
   test(MaxV„rde, SlumpV„rde) if
   time(_,_,_,H),
   SlumpV„rde=H-MaxV„rde,
   SlumpV„rde < Maxv„rde, !,
   SV= abs(Slumpv„rde),
   write("A: ",SV, "\n").
   
   test(MaxV„rde, SlumpV„rde) if
   time(_,_,_,H),
   SlumpV„rde=H/MaxV„rde-1, !,
   SlumpV„rde < Maxv„rde, 
   SV= abs(Slumpv„rde),
   write("B: ",SV, "\n").
   


   