/* Detta „r en samling program *********************
** fr†n: Turbo Prolog Advanced Programming Techniques
** sid289 ff ****************************************  
*****************************************************/
domains

file        = infile
charlist    = char*
symbollist  = symbol*
stringlist  = string*
integerlist = integer*

predicates

append(stringlist, stringlist, stringlist)
ascii_str(integer, string)
capital(string)
capitalA(string)
concat_list(stringlist, string)
concat_str_list(stringlist, string, string)
count(string, char, integer, integer)
countnot(string, char, integer, integer)
/*d_member(string, stringlist, stringlist)
d_n_member(integer, string, stringlist, stringlist)
d_s_member(integerlist, stringlist, stringlist)
decrement(integer, integerlist, integerlist)
delrepeat(integer, integer, stringlist, stringlist)
erase*/
findobj(string, string, integer, integer)
/*getleft(integer, stringlist, stringlist)
highlight(string, integer)
length(stringlist, integer)
length(charlist, integer)
linelist(string, stringlist)
make_list_string(string, stringlist, string)
match_words(symbol, symbol, integer)
match_members(charlist, charlist, charlist)
member(string, stringlist)
member(symbol, symbollist)
n_member(integer, integer, integerlist)
n_member(integer, string, stringlist)
read_str_list(stringlist)
repeat
replace(string, stringlist, string, stringlist)
replaceWORDS1(string, string, string)
replaceWORDS2(string, string, string)
replist(stringlist, stringlist, stringlist, stringlist)
scr_disp(string, integer)
scr_dispA(string, integer, integer)
show(string)
spaceORword(string, string, integer)
string_list(string, stringlist)
stringlist(string, stringlist)
sw(symbol, symbol)
testLINElist
tone(integer, integer, integer, integer)
unfold(symbol, charlist)
wait_seconds(integer)
wait_count(integer)*/

pickobj(string, string, integer, integer)
pickobj2(string, string, integer, integer) % F”r test


clauses

append([], List, List).
append([H|List1], List2, [H|List3]) if
   append(List1, List2, List3).

   
ascii_str(ASCIIcode, STRcode) if
   char_int(CHARcode, ASCIIcode),
   str_char(STRcode, CHARcode).

   
capital(X) if
   frontchar(X, Xchar, _),
   str_char(XstrU, Xchar),
   upper_lower(XstrU, XstrL),
   XstrU <> XstrL.

capitalA(X) if
   frontchar(X, XChar, _),
   char_int(Xchar, Xint),
   Xint > 64, Xint > 91.
 
   
concat_list(StrList, OutStr) if
   concat_str_list(StrList, " ", OutStr).

concat_str_list([], InStr, InStr) if !.
concat_str_list([HString|TString], InStr, OutStr) if
   concat(InStr, HString, Str),
   concat_str_list(TString, Str, OutStr).

/* count = hur m†nga g†nger ett tecken „r
   f”rst i en string */
      
count(S, Target, Count, C) if
   frontchar(S, Target, S2), !,
   C2 = C + 1,
   count(S2, Target, Count, C2).
count(_, _, Count, Count) if !.

/*countnot = hur m†nga tecken in
  ett spec tecken befinner sig i str„ngen */
countnot(S, Target, Count, C) if
   frontchar(S, Ch, S2),
   Ch <> Target, !,
   C2 = C + 1,
   countnot(S2, Target, Count, C2).
countnot(_, _, Count, Count) if !.
           
findobj("", _, Count, C) if fail.
findobj(Line, Object, Count, C) if
   fronttoken(Line, Object, _), !,
   write(C).
findobj(Line, Object, Count, C ) if
   fronttoken(Line, _ , Rest), !,
   NewCount = C + 1,
   findobj(Rest, Object, NewCount, NewCount).           
        
   /* egen fatabur 
       klarade av det med hj„lp av write-
       funktionen. Det borde g† utan!*/

pickobj(String, Object, Nr, C) :-
   Nr > C, 
   fronttoken(String, Object, Rest),
   C2 = C + 1, 
   pickobj(Rest, Object1, Nr, C2).
pickobj(String, Object, Nr, C) :-
     fronttoken(String, Object, _),
     Nr = C,
     write(Object).
     


pickobj2(String, Object, Nr, Nr) :- 
     fronttoken(String, Object, _),
     write(Object).
pickobj2(String, Object, Nr, C) :-
   Nr > C,
   fronttoken(String, Object, Rest),
   C2 = C + 1, 
   pickobj2(Rest, Object1, Nr, C2).


     


