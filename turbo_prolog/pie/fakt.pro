fact( N , F ) :- N > 0 , fact( (N-1) , M ) , F = N * M.
fact( 0 , 1 ).
