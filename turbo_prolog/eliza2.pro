/*     Detta „r ett f”rs”k att g”ra en elizaliknade sak */
predicates
 st_token(string, string, string) /* fr†n TOKENLST.PRO*/
 ch_token(string, char, string)     
 main
 omv„nd(string, string)
 reverse(string, string)
 markers(string, string)
 kolla_markers(string, string)
  
clauses

st_token(StartString, Tok, RestString) if
     fronttoken(StartString, Tok, RestString), 
/*     kolla_markers(Tok, Ut),omv„nd(Tok, Ut_reverse),*/
     st_token(RestString, Tok1, RestString2). 

ch_token(StartString, Char, RestString) if
     frontchar(StartString, Char, RestString), 
     ch_token(RestString, Char1, RestString2). 


omv„nd(Str„ng, Ut) if
   fronttoken(Str„ng, Str, RestStr), 
   reverse(Str, Ut), write(Ut, " "), 
   frontchar(RestStr, Char, RStr),
   omv„nd(RStr, Ut2).
   
omv„nd(Str„ng, _) if 
   write(Str„ng, " "), !.
         
reverse("jag", "du").
reverse("vi", "ni").
reverse("du", "jag").
reverse("ni", "vi").


kolla_markers(Str„ng, Ut) if
  fronttoken(Str„ng, Str, RestStr), !,
  markers(Str, Ut), write(Ut),
  kolla_markers(RestStr, Ut2).
  
kolla_markers(X, "") if !.       

markers("mamma", "badade hon dig aldrig n„r du var liten") if !.
markers("pappa", "l„rde han dig aldrig att skjuta med pistol") if !.
markers("†ngest", "†ngesten „r n†got du m†ste l„ra att hantera sj„lv") if !.
markers(X,"") if !.
     
main if
   readln(Input),
   kolla_markers(Input, X), write(X),
   omv„nd(Input,Y).
   
     


      
