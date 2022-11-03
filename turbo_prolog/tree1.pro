domains
	treetype = tree(string, treetype, treetype); nil
	

predicates

      /*  traverse(treetype, treetype, treetype) */
        print_all_elements(treetype)


clauses

print_all_elements(nil).
print_all_elements(tree(X, Y, Z)) if
       write(X), nl,
       print_all_elements(Y),
       print_all_elements(Z).
       
       
       	       

goal

   print_all_elements(tree("Cathy",
        	tree("Michael",
        		tree("Charles", nil, nil),
        		tree("Hazel", nil, nil)),
        tree("Melody",
        	tree("Jim", nil, nil),
        	tree("Eleanor", nil, nil)))).
        	
/*        	
traverse(nil).
traverse(tree(X,Y,Z) if
	do_something_with_X,
	traverse(Y),
	traverse(Z).
*/
	
	        			




