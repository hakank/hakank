domains

       name = symbol 
       
predicates

	sister_of(name, name)
	father_of(name, name)
	mother_of(name, name)
	daughter_of(name, name)
	son_of(name, name)
	brother_of(name, name)

	female(name)
	parents(name,name,name)
	male(name)

clauses
			
sister_of(X,Y) if
	female(X), 
	parents(X,M,F), 
	parents(Y,M,F), not (X=Y).

brother_of(X,Y) if
	male(X), 
	parents(X,M,F), 
	parents(Y,M,F), not (X=Y).



father_of(X,Y) if 
	parents(Y,_,X).

mother_of(X,Y) if
	parents(Y,X,_).
 	
daughter_of(X,Y) if
	female(X),
	parents(X,Y,_).
		
daughter_of(X,Y) if
	female(X),
	parents(X,_,Y). 

son_of(X,Y)	if
	male(X),
	parents(X,Y,_).
	
son_of(X,Y)	if
	male(X),
	parents(X,_,Y).


male(albert).
male(edward).
male(kalle).

female(alice).
female(victoria).
female(sofia).
female(ulrika).

parents(edward, victoria, albert).
parents(alice, victoria, albert).
parents(kalle, victoria, albert).
parents(sofia, victoria, nisse).
parents(ulrika, victoria, nisse).

/*
goal
	sister_of(X,Y),
	write (X), write (" - "), 
        write (Y), 
	nl, nl,
	fail.		
		
*/		


	
	
