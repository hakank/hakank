% https://open.kattis.com/problems/anewalphabet
% 1s
% 1.7 Easy

% Is it reversible? Jup!

main :-
    read_line_to_string(user_input,S0),
    string_lower(S0,S1),
    writeln(S1),
    string_chars(S1,S),
    s(S,[],R),
    maplist(format('~w'),R),
    nl,
    writeln("\nReverse:"),
    s(Rev,[],R),
    format('~s~n',[Rev]),
    % But this prints "perhaps_not" !
    (not(Rev \= S1) -> writeln(ok) ; writeln(perhaps_not)).
main.

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
