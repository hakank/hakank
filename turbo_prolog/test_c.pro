/* test p† komp mellan prolog och c */
/* sid 84 i ref guide */
global predicates
	message(string) - (i) language c
	hello_c - language c
	
clauses
	message(S) :-
	makewindow(13, 7, 7, " F”nster", 10, 10, 3, 50),
	write(S), readchar(_),
	removewindow.
goal
	message("Hello from Turbo Prolog!"),
	hello_c.
		
	