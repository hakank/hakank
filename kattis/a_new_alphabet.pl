% https://open.kattis.com/problems/anewalphabet
% 1s
% 1.7 Easy

% First version got a Time Limit Exeeded on this (using append/3)
%% s([],S,S).
%% s([H|T],S0,S) :-
%%     s(H,C),
%%     append(S0,[C],S1),
%%     s(T,S1,S).

% Fixed.
% And it's reversible: see a_new_alphabet2.pl 
   
main :-
    read_line_to_string(user_input,S0),
    string_lower(S0,S1),
    string_chars(S1,S),
    s(S,[],R),
    flatten(R,Res),
    maplist(format('~w'),Res),
    nl.

s([],S,S).
s([H|T],S0,[C|S]) :-
    s(H,C),
    s(T,S0,S).

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
