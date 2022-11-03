predicates

test(real, real, real, real)

main
      
clauses
   

  test(X,Y, A, B) if
  A=((Y*Y)-1)/2, 
  B=X+1,
  write(A, " ", B, "\n"),
  NewX=X+1, NewY=Y+X,
  test(NewX, NewY,NewA, NewB).


   main   if 
       test(1,1,A, B).
  
  






 