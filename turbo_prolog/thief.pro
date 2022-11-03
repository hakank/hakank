predicates
	may_steal(symbol,symbol)
	thief(symbol)
	likes(symbol,symbol)
	valuable(symbol)
	
clauses
	may_steal(P,T) if
		thief(P),
		likes(P,T),
		valuable(T).
		
thief(kalle).
thief(olle).
thief(nisse).

likes(kalle, cykel).
likes(kalle, b„r).
likes(olle, qwerty).
likes(olle, cykel).
likes(nisse, qwerty).
likes(eva, X) if likes (nisse,X).


valuable(cykel).
	valuable(T) if 
	likes(nisse,T).
	

			
		
		
			

