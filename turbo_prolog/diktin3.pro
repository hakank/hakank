/************************ BORING POEMINSPIRATOR **********************
 Ett program som genererar slumpmeningar med "poetiska ord" f”r att
 man sedan ska kunna f† en inspiration och sedan skriva en liten dikt
 om „mnet som kommit upp p† sk„rmen!
  
 Nu har jag gjort s† att programmet g†r runt, och l„gger sig i
 PROLOG.LOG, som jag sedan d”per om till n†got. 
 Jag har redan DIKT1.EXP!, och DIKT2.exp

  Kravspec: utveckla s† att adjektiv „ven kommer in!
 
 (c) 1989 Boring Inc, Int (R)
 *********************************************************************/ 			
domains
 stringlist = string* 

predicates

medd(integer, integer, integer, integer, integer, integer, integer, integer, integer, integer)
meddel(integer, integer, integer, integer, integer, integer, integer, integer, integer, integer)
meddelande1(string, string, string, string, string, string, string, string, string, integer, integer, integer, integer, integer, string, string, string, string)
meddelande2(string, string, string, string, string, string, string, string, string, integer, integer, integer, integer, integer, string, string, string, string)
meddelande3(string, string, string, string, string, string, string, string, string, integer, integer, integer, integer, integer, string, string, string, string)
meddelande4(string, string, string, string, string, string, string, string, string, integer, integer, integer, integer, integer, string, string, string, string)
subst_n(integer, string)
subst_t(integer, string)
subst_de(integer, string)
subst(integer, string)
adj_n(integer, string)
adj_t(integer, string)
verb(integer, string)
rest(integer, string)
aux(integer, string)
main
finn_alla_subst_n(stringlist, integer)
finn_alla_subst_t(stringlist, integer)
finn_alla_subst_de(stringlist, integer)
finn_alla_subst(stringlist, integer)
finn_alla_adj_n(stringlist, integer)
finn_alla_adj_t(stringlist, integer)
finn_alla_verb(stringlist, integer)
finn_alla_rest(stringlist, integer)
finn_alla_aux(stringlist, integer)
length(stringlist, integer)

      
clauses

medd(Num, Adj_n_max, Adj_t_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax) if
  random(4, Num),
  meddel(Num, Adj_n_max, Adj_t_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax),
  medd(Num1, Adj_n_max, Adj_t_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax).
  
meddel(0, Adj_n_max, Adj_n_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax) if 
     meddelande1(Adj_n, Adj_t, S_n, S_t, S_de, S, V, R, A, Adj_t_max, Adj_n_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax), !.
meddel(1, Adj_n_max, Adj_t_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax) if
     meddelande2(Adj_n, Adj_t, S_n, S_t, S_de, S, V, Adj_n_max, Adj_t_max, S_n_max, S_t_max, Smax, Vmax), !.
meddel(2, Adj_n_max, Adj_n_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax) if 
     meddelande3(Adj_n, Adj_t, S_n, S_t, S_de, S, V, R, A, Adj_t_max, Adj_n_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax), !.
meddel(3, Adj_n_max, Adj_t_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax) if
     meddelande4(Adj_n, Adj_t, S_n, S_t, S_de, S, V, Adj_n_max, Adj_t_max, S_n_max, S_t_max, Smax, Vmax), !.


meddelande1(Adj_n, Adj_t, S_n, S_t, S_de, S, V, R, A, Adj_n_max, Adj_t_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax) if
    random(Adj_n_max, Adj_n_num),
    random(S_n_max, Snum), random(Vmax, Vnum),
    random(Rmax, Rnum), 
    adj_n(Adj_n_num, Adj_n),
    subst_n(Snum, S_n), verb(Vnum, V), rest(Rnum, R),
    write(Adj, S_n, V, R),
    random(Amax, Anum2), random(Adj_n_max, Adj_n_num2),
    random(S_n_max, S_n_num2),
    random(Vmax, Vnum2), random(Rmax, Rnum2), 
    aux(Anum2, A2), adj(Adjnum2, Adj2),
    subst_n(S_n_num2, S2), verb(Vnum2, V2),
    rest(Rnum2, R2),
    upper_lower(S2, LowerS2),
    write(A2, Adj2, LowerS2, V2, R2, ".\n").
    
meddelande3(Adj_n, Adj_t, S_n, S_t, S_de, S, V, R, A, Adj_n_max, Adj_t_max, S_n_max, S_t_max, S_de_max, Smax, Vmax, Rmax, Amax) if
    random(Adj_t_max, Adj_t_num),
    random(S_t_max, Snum), random(Vmax, Vnum),
    random(Rmax, Rnum), 
    adj_t(Adjnum, Adj),
    subst_t(Snum, S), verb(Vnum, V), rest(Rnum, R),
    write(Adj, S, V, R),
    random(Amax, Anum2), random(Adj_t_max, Adj_t_num2),
    random(S_t_max, Snum2),
    random(Vmax, Vnum2), random(Rmax, Rnum2), 
    aux(Anum2, A2), adj_t(Adjnum2, Adj2),
    subst_t(Snum2, S2), verb(Vnum2, V2),
    rest(Rnum2, R2),
    upper_lower(S2, LowerS2),
    write(A2, Adj2, LowerS2, V2, R2, ".\n").

meddelande2(Adj, S, V, Adjmax, Smax, Vmax) if
    random(Adj_n_max, Adj_n_num),
    random(S_n_max, Snum),
    random(Vmax, Vnum), 
    adj_n(Adj_n_num, Adj),
    subst_n(Snum, S), verb(Vnum, V),
    write(Adj, S, V, ".\n").
meddelande4(Adj, S, V, Adj_t_max, Smax, Vmax) if
    random(Adj_t_max, Adjnum),
    random(S_t_max, Snum),
    random(Vmax, Vnum), 
    adj_t(Adj_t_num, Adj),
    subst_t(Snum, S), verb(Vnum, V),
    write(Adj_t, S_t, V, ".\n").
    

main if
   finn_alla_subst_n(L, S_n_num),
   finn_alla_subst_t(L, S_t_num),
   finn_alla_subst_de(L, S_de_num),
   finn_alla_subst(L, Snum),
   finn_alla_adj_n(L, Adj_n_num),
   finn_alla_adj_t(L, Adj_t_num),
   finn_alla_verb(L, Vnum),
   finn_alla_rest(L, Rnum),
   finn_alla_aux(L, Anum),
   medd(X, Adj_n_num, Adj_t_num, S_n_num, S_t_num, S_de_num, Snum, Vnum, Rnum, Anum).

finn_alla_subst_n(L, Len) if
  findall(String, subst_n(_, String), L1),
  length(L1, Len).
finn_alla_subst_t(L, Len) if
  findall(String, subst_t(_, String), L1),
  length(L1, Len).
finn_alla_subst_de(L, Len) if
  findall(String, subst_de(_, String), L1),
  length(L1, Len).
finn_alla_subst(L, Len) if
  findall(String, subst(_, String), L1),
  length(L1, Len).
finn_alla_adj_n(L, Len) if
  findall(String, adj_n(_, String), L1),
  length(L1, Len).
finn_alla_adj_t(L, Len) if
  findall(String, adj_n(_, String), L1),
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

adj_t(0,"Det vackra ").
adj_n(1,"Den r„dda ").

subst_n(0,"mj”lken").
subst_n(1,"pennan").
subst_n(2,"kulramen").
subst_n(3,"spegeln").
subst_n(4,"f†geln").
subst_n(5,"l„rkan").
subst_n(6,"boken").
subst_n(7,"f”r„lskade kvinnan").
subst_n(8,"staven").
subst_n(9,"gr„nsen").
subst_n(10,"dikten").
subst_n(11,"stenen").
subst_n(12,"virveln").
subst_n(13,"”gonen").
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

subst(0,"din hand").
subst(1,"du").
subst(2,"jag").
subst(3,"den andre").
subst(4,"hon").
subst(5,"eviga").

subst_de(0,"v†gorna").

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
verb(36," blir en symbol ").

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
rest(20,"under min hud").
rest(21,"bredvid min kudde").
rest(22,"vid stearinljuset").
rest(23,"p† ett riktigt s„tt").


aux(0, " ehuru ").
aux(1, " d„rf”r att ").
aux(2, " om ").
aux(3, " n„r ").
aux(4, " men ").
aux(5, " eller ").
aux(6, " varf”r ").
aux(7, " ist„llet f”r att ").


/*goal
    main.*/

