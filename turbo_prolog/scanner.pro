/*         **********   SCANNER   ************
     Nu har jag „ndrat denna ”vning (CH13EX03.PRO, sid 287)
     till att g”ra saker med ordklasser ist„llet!
     En bit p† v„gen.  
     Problemet „r ju alltid att skriva ned alla m”jligheterna.
     (En mer avancerad version finns ju alltid i SEN_AN.PRO.)
****************************************************************/

domains
   tok  = subst(symbol); verb(symbol); 
          adj(symbol); deter(symbol);
          avskilj(symbol);ok„nt(symbol);
          siffra(symbol)
   toklist = tok*

predicates
   scanner(string, toklist)
   maketok(string, tok)
   verb(string)
   adj(string)
   subst(string)
   deter(string)
   avskilj(string)
   siffra(string)
   ok„nt(string)
   
clauses
   
   scanner("", []).
   scanner(Str, [Tok|Rest]) :-
      fronttoken(Str, Sym, Str1), 
      maketok(Sym, Tok), 
      scanner(Str1, Rest).

maketok(S, verb(S))  :- verb(S).
maketok(S, subst(S)) :- subst(S).
maketok(S, adj(S))   :- adj(S).
maketok(S, deter(S)) :- deter(S).
maketok(S, avskilj(S)) :- avskilj(S).
maketok(S, siffra(S)) :- siffra(S).
maketok(S, ok„nt(S)) :- ok„nt(S).

avskilj(".").
avskilj(",").
avskilj("?").
avskilj(";").
avskilj(":").
verb(„ta).
verb(„ter).
verb(skiter).
verb(skita).
verb(sova).
verb(„ter).
verb(sover).
verb(„r).
subst(jag).
subst(du).
subst(dig).
subst(han).
adj(vackert).
adj(d†ligt).
deter(att).
deter(det).
deter(den).
deter(eller).
siffra("1").
siffra("2").
siffra("3").
siffra("4").
siffra("5").
siffra("6").
siffra("7").
siffra("8").
siffra("9").
siffra("0").

ok„nt(_).

goal
   write("Enter some text:"),nl,
   readln(Text),nl,
   scanner(Text,T_List),
   write(T_List).
   
   
   
   
   