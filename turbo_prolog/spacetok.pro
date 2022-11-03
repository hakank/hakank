
domains
   file    =Ut_fil
       
predicates

	space_token(string, char, string)
	
clauses
	
	
	space_token(Input, Char, RestString) if
	     frontchar(Input, Char, RestString2), write(Char," "), 
 	     writedevice(ut_Fil),
 	     space_token(RestString2, Char1, Reststring3). 

goal
	write("Detta „r en space_tokenizer.\n"),
	write("Den l„gger ett mellanslag mellan varje bokstav.\n"),
 	write("OBS! I denna beta-version f†r ut-filen namnet\n"),
 	write("OUTPUT.TOK\n"),
        write("Vilken fil vill du space_tokenize?: (Tryck Enter se'n.)\n"),
        readln(In_Fil),  
	file_str(In_Fil, Text),
	openwrite(ut_fil, "output.tok"),
	space_token(Text, Char, RestString),
	writedevice(screen), write("hejd†").
	
	  
/*   
read_input_text_in_file
do_space_token
show_file_on_screen
(save_file_with extention_.spc)
closefile(FileName)

*/