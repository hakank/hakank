/************************ BORING POEMINSPIRATOR **********************
 Ett program som genererar slumpmeningar med "poetiska ord" f”r att
 man sedan ska kunna f† en inspiration och sedan skriva en liten dikt
 om „mnet som kommit upp p† sk„rmen!
  
 Nu har jag gjort s† att programmet g†r runt, och l„gger sig i
 PROLOG.LOG, som jag sedan d”per om till n†got. 
 Jag har redan DIKT1.EXP!, och DIKT2.exp

  Kravspec: utveckla s† att adjektvi „ven kommer in!
 
 (c) 1989 Boring Inc, Int (R)
 *********************************************************************/ 
domains
 stringlist = string* 

predicates
meddelande(string, string, string, string, integer, integer, integer, integer)
subst(integer, string)
verb(integer, string)
rest(integer, string)
aux(integer, string)
main
finn_alla_subst(stringlist, integer)
finn_alla_verb(stringlist, integer)
finn_alla_rest(stringlist, integer)
finn_alla_aux(stringlist, integer)
length(stringlist, integer)
      
clauses
   
meddelande(S, V, R, A, Smax, Vmax, Rmax, Amax) if
    random(Smax, Snum),random(Vmax, Vnum),
    random(Rmax, Rnum), 
    subst(Snum, S), verb(Vnum, V), rest(Rnum, R),
    write(S, V, R),
    random(Amax, Anum2), random(Smax, Snum2),
    random(Vmax, Vnum2), random(Rmax, Rnum2), 
    aux(Anum2, A2), subst(Snum2, S2), verb(Vnum2, V2), rest(Rnum2, R2),
    upper_lower(S2, LowerS2),
    write(A2, LowerS2, V2, R2, ".\n"),
    meddelande(S3, V3, R3,A3, Smax, Vmax, Rmax, Amax).

main if
   finn_alla_subst(L, Snum),
   finn_alla_verb(L, Vnum),
   finn_alla_rest(L, Rnum),
   finn_alla_aux(L, Anum),
   meddelande(S, V, R, A, Snum, Vnum, Rnum, Anum).
/*   write(S,V, R,"\n"),
   readln(X),
   X<>"slut", 
   main.*/

finn_alla_subst(L, Len) if
  findall(String, subst(_, String), L1),
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

  length([],0).
  length([_|T], L) if
     length(T, L1),
     L=L1+1. 

subst(0,"Ansiktet").
subst(1,"Pennan").
subst(2,"Kulramen").
subst(3,"Spegeln").
subst(4,"F†geln").
subst(5,"L„rkan").
subst(6,"Boken").
subst(7,"Den f”r„lskade kvinnan").
subst(8,"Staven").
subst(9,"Gr„nsen").
subst(10,"Dikten").
subst(11,"Spr†ket").
subst(12,"Stenen").
subst(13,"L”vet").
subst(14,"Virveln").
subst(15,"Tr„det").
subst(16,"H†ret").
subst(17,"™gonen").
subst(18,"Dalen").
subst(19,"Berget").
subst(20,"Staden").
subst(21,"Ordet").
subst(22,"K„nslan").
subst(23,"N†gon").
subst(24,"Molnet").
subst(25,"Tekoppen").
subst(26,"Din hand").
subst(27,"Tonen").
subst(28,"V†gorna").
subst(29,"Livet").
subst(30,"Jorden").
subst(31,"Solen").
subst(32,"Planeten").
subst(33,"Vinet").
subst(34,"Mj”lken").
subst(35,"Du").
subst(36,"Jag").
subst(37,"Den andre").
subst(38,"Hon").
subst(39,"Spekulationen").
subst(40,"Det eviga").


verb(0," svarar ").
verb(1," ramlar ").
verb(2," faller ").
verb(3," f”r„lskar sig ").
verb(4," blir funnen ").
verb(5," saknas ").
verb(6," finns ").
verb(7," ligger ").
verb(8," sitter ").
verb(9," flyter ").
verb(10," simmar ").
verb(11," v†gar ").
verb(12," „lskar ").
verb(13," vill ").
verb(14," str„var ").
verb(15," strider ").
verb(16," lovar ").
verb(17," ljuger ").
verb(18," talar sanning ").
verb(19," diktar ").
verb(20," talar ").
verb(21," f”rvandlar sig ").
verb(22," f”rvarar ").
verb(23," d”ljer ").
verb(24," liknar ").
verb(25," rymmer ").
verb(26," b”jer sig ").
verb(27," stannar kvar ").
verb(28," blir f†ngad ").
verb(29," d”ljer det ").
verb(30," d”ljer sig ").
verb(31," f”rvandlar det ").
verb(32," saknar det ").
verb(33," v†rdar ").
verb(34," ”nskar ").
verb(35," s†rar ").
verb(36," blir en symbol f”r ").
verb(37," ").

rest(0,"vid brunnen").
rest(1,"i skogen").
rest(2,"pl”tsligt").
rest(3,", men inte idag").
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
rest(20,"").

aux(0, " ehuru ").
aux(1, " d„rf”r att ").
aux(2, " om ").
aux(3, " n„r ").
aux(4, " men ").
aux(5, " eller ").
aux(6, " varf”r ").
aux(7, " ist„llet f”r att ").


goal
    main.

