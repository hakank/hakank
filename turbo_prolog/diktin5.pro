/************************ BORING POEMINSPIRATOR **********************
 Ett program som genererar slumpmeningar med "poetiska ord" f”r att
 man sedan ska kunna f† en inspiration och sedan skriva en liten dikt
 om „mnet som kommit upp p† sk„rmen!
  
Detta gjordes 89-11-05 (rev 89-11-08)
Och „r en utveckling av de gamla.
Det g†r inte runt eftersom det blir en stack overflow.
  prova sentence(X) eller
  main eller
  main1

OBS! Jag har f”r„ndrat slumpgenerator till exe filen!!!!!!!!


DETTA HAR BLIVIT EN .EXE-fil DIKTIN5.EXE!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 (c) 1989 Boring Inc, Int (R)
 *********************************************************************/ 			
domains
 stringlist = string* 

predicates

subst(integer, string)
subst_n(integer, string)
subst_t(integer, string)
subst_de(integer, string)
verb(integer, string)
rest(integer, string)
aux(integer, string)
adj(integer, string)
main
main1(string)
main2
main3
finn_alla_subst(stringlist, integer)
finn_alla_subst_n(stringlist, integer)
finn_alla_subst_t(stringlist, integer)
finn_alla_subst_de(stringlist, integer)
finn_alla_verb(stringlist, integer)
finn_alla_rest(stringlist, integer)
finn_alla_aux(stringlist, integer)
finn_alla_adj(stringlist, integer)
length(stringlist, integer)

finn_adj(string) 
finn_subst(string) 
finn_subst_n(string) 
finn_subst_t(string) 
finn_subst_de(string) 
finn_verb(string) 
finn_rest(string) 
finn_aux(string) 

sentence(integer)
slump(integer,integer)

      
clauses

sentence (0) if
    finn_subst_n(S), finn_verb(V), finn_rest(R), finn_aux(A), 
    finn_subst(S_1),
    finn_verb(V1), finn_rest(R1),
    finn_subst_de(Subst_de), finn_rest(Rest2),
    write(S, " ", V, " ", R, " ", A, " ", S_1, " ", V1," ", R1, ". ", Subst_de, " ", Rest2, ".\n").
    
sentence (1) if
    finn_adj(Adj), 
    finn_subst_t(S), finn_verb(V), finn_aux(A), 
    finn_subst_n(S_1),
    finn_verb(V1),
    write("det ", Adj, " ", S, " ", V," ", A," ", S_1," ",V1, ".\n").
    
sentence (2) if
    finn_adj(Adj),
    finn_subst_n(S), finn_verb(V), finn_rest(R),
    finn_subst(S1), 
    finn_rest(R1), 
    write("den ", Adj, " ",  S, " ",V," ", R, ". ", S1, " ", R1, ".\n").

sentence (3) if
    finn_subst_n(S), finn_rest(R),
    finn_subst(S1), 
    finn_rest(R1), 
    write(S, " ", R, ". ", S1, " ", R1, ".\n").

sentence (4) if
    finn_rest(Rest),
    finn_rest(Rest1), 
    write(Rest, ". ", Rest1, ".\n").

sentence (5) if
    finn_adj(Adj),
    finn_subst_n(S), finn_verb(V), finn_rest(R),
    finn_subst_n(S1), 
    finn_rest(R1), 
    write("den ", Adj, " ", S,  " ",V, " ",  R, ". ", S1, " ", R1, ".\n").


finn_subst(Subst) if
    finn_alla_subst(L, Smax),
    slump(Smax, Snum),
    subst(Snum, Subst).
finn_subst_n(Subst) if
    finn_alla_subst_n(L, Smax),
    slump(Smax, Snum),
    subst_n(Snum, Subst).
finn_subst_t(Subst) if
    finn_alla_subst_t(L, Smax),
    slump(Smax, Snum),
    subst_t(Snum, Subst).
finn_subst_de(Subst) if
    finn_alla_subst_de(L, Smax),
    slump(Smax, Snum),
    subst_de(Snum, Subst).
    
finn_verb(Verb) if
    finn_alla_verb(L, Vmax),
    slump(Vmax, Vnum),
    verb(Vnum, Verb).
    
finn_rest(Rest) if
    finn_alla_rest(L, Rmax),
    slump(Rmax, Rnum),
    rest(Rnum, Rest).
    
finn_aux(Aux) if
    finn_alla_aux(L, Amax),
    slump(Amax, Anum),
    aux(Anum, Aux).

finn_adj(Aux) if
    finn_alla_adj(L, Amax),
    slump(Amax, Anum),
    adj(Anum, Aux).

finn_alla_subst(L, Len) if
  findall(String, subst(_, String), L1),
  length(L1, Len).
finn_alla_subst_n(L, Len) if
  findall(String, subst_n(_, String), L1),
  length(L1, Len).
finn_alla_subst_t(L, Len) if
  findall(String, subst_t(_, String), L1),
  length(L1, Len).
finn_alla_subst_de(L, Len) if
  findall(String, subst_de(_, String), L1),
  length(L1, Len).
finn_alla_verb(L, Len) if
  findall(String, verb(_, String), L1),
  length(L1, Len).
finn_alla_rest(L, Len) if
  findall(String, rest(_, String), L1),
  length(L1, Len).
finn_alla_aux(L, Len) if
  findall(String, aux(_, String), L1),
  length(L1, Len).
finn_alla_adj(L, Len) if
  findall(String, adj(_, String), L1),
  length(L1, Len).

length([],0).
length([_|T], L) if
     length(T, L1),
     L=L1+1. 

     
adj(0,"vackra").
adj(1,"r„dda").
adj(2,"lilla").
adj(3,"underbara").
adj(4, "sj„lvklara").
adj(5, "f”r„lskade").

subst_n(0,"mj”lken").
subst_n(1,"pennan").
subst_n(2,"kulramen").
subst_n(3,"spegeln").
subst_n(4,"f†geln").
subst_n(5,"l„rkan").
subst_n(6,"boken").
subst_n(7,"kvinnan").
subst_n(8,"staven").
subst_n(9,"gr„nsen").
subst_n(10,"dikten").
subst_n(11,"stenen").
subst_n(12,"virveln").
subst_n(13,"”gonvr†n").
subst_n(14,"dalen").
subst_n(15,"staden").
subst_n(16,"k„nslan").
subst_n(17,"n†gon").
subst_n(18,"tekoppen").
subst_n(19,"spekulationen").
subst_n(20,"tonen").
subst_n(21,"jorden").
subst_n(22,"solen").
subst_n(23,"planeten").

subst_t(0,"spr†ket").
subst_t(1,"ansiktet").
subst_t(2,"l”vet").
subst_t(3,"tr„det").
subst_t(4,"h†ret").
subst_t(5,"berget").
subst_t(6,"ordet").
subst_t(7,"molnet").
subst_t(8,"livet").
subst_t(9,"vinet").
subst_t(10,"”gat").

subst(0,"din hand").
subst(1,"du").
subst(2,"jag").
subst(3,"den andre").
subst(4,"hon").
subst(5,"eviga").

subst_de(0,"v†gorna").
subst_de(1,"haven").
subst_de(2,"f†glarna").
subst_de(3,"l”ven").
subst_de(4,"”gonen").

verb(0,"svarar").
verb(1,"ramlar").
verb(2,"faller").
verb(3,"f”r„lskar sig").
verb(4,"blir funnen").
verb(5,"saknas").
verb(6,"finns").
verb(7,"ligger").
verb(8,"sitter").
verb(9,"flyter").
verb(10,"simmar").
verb(11,"v†gar").
verb(12,"„lskar").
verb(13,"vill").
verb(14,"str„var").
verb(15,"strider").
verb(16,"lovar").
verb(17,"ljuger").
verb(18,"talar sanning").
verb(19,"diktar").
verb(20,"talar").
verb(21,"f”rvandlar sig").
verb(22,"f”rvarar").
verb(23,"d”ljer").
verb(24,"liknar").
verb(25,"rymmer").
verb(26,"b”jer sig").
verb(27,"stannar kvar").
verb(28,"blir f†ngad").
verb(29,"d”ljer det").
verb(30,"d”ljer sig").
verb(31,"f”rvandlar det").
verb(32,"saknar det").
verb(33,"v†rdar").
verb(34,"”nskar").
verb(35,"s†rar").
verb(36,"blir en symbol").

rest(0,"vid brunnen").
rest(1,"i skogen").
rest(2,"pl”tsligt").
rest(3,"men inte idag").
rest(4,"kanske").
rest(5,"p† Venus").
rest(6,"vid sidan om det").
rest(7,"om kr”net").
rest(8,"kraftigt").
rest(9,"svagt").
rest(10,"ovanp†").
rest(11,"anonymt").
rest(12,"trasigt").
rest(13,"sprucket").
rest(14,"n„ra din hand").
rest(15,"vid din hud").
rest(16,"genom spr†ket").
rest(17,"under tiden").
rest(18,"meningsl”st").
rest(19,"fast").
rest(20,"under min hud").
rest(21,"bredvid min kudde").
rest(22,"vid stearinljuset").
rest(23,"p† ett riktigt s„tt").


aux(0,"ist„llet f”r att").
aux(1,"d„rf”r att").
aux(2,"om").
aux(3,"n„r").
aux(4,"men").
aux(5,"eller").
aux(6,"varf”r").

main if 
      comline(Inp1),
      main1(Inp1).

main1(X) if
     X<>"",
     str_int(X, X_int),  
     X_int<=5,
     sentence(X_int).

main1(X) if
     str_int(X, X_int), !, 
     X_int > 5,
     write("I didn't tell you to write this number, did I?\n").

main1(X)  if
     X<>"", !,
     main2.
     
main1(X) if 
      system("CLS"),
      write("******** Welcome to BORING POEM GENERATOR GEN_1 (C) (R) *********\n"),
      write("******** presented proudly by BORING Int Inc (C) (R)    *********\n\n"),
      write("Write DIKTIN5 [command]\n"),
      write("where [command] could be:\n"),
      write("1 ... 5 \n"),
      write("or poems\n"),      
      write("where 1 ... 5  is the different sort of sentences\n"),
      write("Try for yourself and be poetric!\n\n").
      
main2  if
     write("BORINGS P.G. GEN_1 s„ger (med djup r”st):\n\n"),
     main3, main3, main3.     

main3  if
   time(_,_,_,H),
   H1=H/20, H2=round(H1),
   sentence(H2).

   slump(MaxV„rde, SV) if
   time(_,_,_,H),
   SlumpV„rde=H*MaxV„rde/100-1,
   SlumpV„rde < Maxv„rde, 
   SV= abs(Slumpv„rde),
   SV1=trunc(SV).
   
   slump(MaxV„rde, SV) if
   time(_,_,H, _),
   SlumpV„rde=H/MaxV„rde-1, !,
   SlumpV„rde < Maxv„rde, 
   SV= abs(Slumpv„rde).
   
goal
      main.
      
    