domains
	treetype = tree(string, treetype, treetype); nil()
	

predicates

   create_tree(string, treetype)
   insert_left(treetype, treetype, treetype)
   insert_right(treetype, treetype, treetype)
 
clauses

   create_tree(A, tree(A, nil, nil)).
   insert_left(X, tree(A, _, B), tree(A, X, B)).
   insert_right(X, tree(A, B, _), tree(A, B, X)).
   
goal

   create_tree("Charles", Ch),
   create_tree("Hazel", Ha),	
   create_tree("Michael", Mi),		        			
   create_tree("Jim", J),	
   create_tree("Eleanor", E),		        			
   create_tree("Melody", Me),	
   create_tree("Cathy", Ca),
   
   insert_left(Ch, Mi, Mi2),
   insert_right(H, Mi2, Mi3),
   insert_left(J, Me, Me2),
   insert_right(E, Me2, Me3),
   insert_left(Mi3, Ca, Ca2),
   insert_right(Me3, Ca2, Ca3),
   
   write(Ca3), nl
   
