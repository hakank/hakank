/*********************************************************************
            Mina f”rsta f”rs”k till str„nghantering
                       1989-10-10  
            (c) H†kan Kjellerstrand            
 *********************************************************************/
      



predicates

	st_token(string, string, string)
	ch_token(string, char, string)
	start(string)
	start1	
	space_token(string, char, string)

clauses

/*  st_token (stringtoken) delar alla ord och skriver ' f”re och efter ordet.
    Žven komma (,) och punkt (.) r„knas som token h„r!*/ 

	st_token(X, Tok, RestString) if
	     fronttoken(X, Tok, RestString), write("'",Tok, "' "), 
             st_token(RestString, Tok1, RestString2). 

/* ch_token (charactertoken) g”r samma fast med bokst„ver */

	ch_token(X, Char, RestString) if
	     frontchar(X, Char, RestString), write("'",Char, "' "), 
 	     ch_token(RestString, Char1, RestString2). 

/* space_char inneb„r att man f†r varje bokstav separerad med ett mellanslag,
   n†got som jag var i behov av  n„r jag h”ll p† med makron i Cicero.*/

	space_token(X, Char, RestString) if
	     frontchar(X, Char, RestString), write(Char," "), 
 	     space_token(RestString, Char1, RestString2). 



/* De tv† starten inneb„r att man f”rst skriver in en str„ng,
   f†r stringtoken p† det, och sedan f†r charactertoken p† det.
   Jag blev tvungen att s„tta dit en extra clausul (start1) f”r att detta
   skulle lyckas*/
    
 start(X) if
      write("\nString_token ger:\n"), st_token(X, Tok, I_String).
      
 start(X) if
      write("\nCharacter_token ger:\n"),ch_token(X, Char, I_String).

 start(X) if 
      write("\nSpace_token ger:"),space_token(X, Char, I_String).
  
 start1 if
 readln(InputString), start(InputString).


goal 
start1.


/* De olika str„ng-kommandona:

frontchar(String,FrontChar,RestString)
	(string,char,string) - (i,o,o) (i,i,o)
	(i,o,i) (i,i,i) (o,i,i)
fronttoken(String,Token,RestString)
	(string,string,string) - (i,o,o) (i,i,o)
	(i,o,i) (i,i,i) (o,i,i)
frontstr(Lenght,Inpstring,StartString,RestString)
	(integer,string,string,string) - (i,i,o,o)
concat(String1,String2,String3)  String3 = String1 + String2
	(string,string,string) - (i,i,o) (i,o,i)
	(o,i,i) (i,i,i)
str_len(String,Length)
	(string,integer) - (i,i) (i,o) (o,i)
isname(StringParam)
	(string) - (i)
format( OutputVariable, FormatString, Variable|Constant* )

*/


