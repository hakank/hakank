domains
	name, book_title, author = symbol
        pages                    = integer
predicates

	owns(name, book_title, author, pages)
	big_book(book_title)
clauses

big_book(X)  if
	owns(_,X,_,Y), Y >500.

owns(john, wuthering_heights, bronte, 600).
owns(mary, moby_dick, melville, 200).
owns(kalle, br”derna_karamasov, dostovjevskij, 900).




goal
/*	owns(Name, Book_title, Author, Pages),
	write (Name, " „ger ", Book_title),
	nl, fail.
*/
	big_book(Book_title), 
	owns(Name, Book_title, _,_), nl,
	write(Name), write(" „ger den feta boken "), nl,
	write(Book_title), fail.
	
	
		 
	 