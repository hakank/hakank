% https://open.kattis.com/problems/anewalphabet
% 1s
% 1.7 Easy

% Shorter variant using a list lookup.

main :-
    read_line_to_string(user_input,S0),
    string_lower(S0,S1),string_chars(S1,S),
    s(L),
    findall(C,(member(A,S),once(member([A,C],L);C=A)),Cs),    
    maplist(format('~w'),Cs),
    nl.
main.
s([[a,"@"],[n,"[]\\[]"],[b,"8"],[o,"0"],[c,"("],[p,"|D"],[d,"|)"],[q,"(,)"],
[e,"3"],[r,"|Z"],[f,"#"],[s,"$"],[g,"6"],[t,"']['"],[h,"[-]"],[u,"|_|"],[i,"|"],
[v,"\\/"],[j,"_|"],[w,"\\/\\/"],[k,"|<"],[x,"}{"],[l,"1"],[y,"`/"],[m,"[]\\/[]"],[z,"2"]]).


s(a,"@").
s(n,"[]\\[]").
s(b,"8").
s(o,"0").
s(c,"(").
s(p,"|D").
s(d,"|)").
s(q,"(,)").
s(e,"3").
s(r,"|Z").
s(f,"#").
s(s,"$").
s(g,"6").
s(t,"']['").
s(h,"[-]").
s(u,"|_|").
s(i,"|").
s(v,"\\/").
s(j,"_|").
s(w,"\\/\\/").
s(k,"|<").
s(x,"}{").
s(l,"1").
s(y,"`/").
s(m,"[]\\/[]").
s(z,"2").
s(X,X).

