kommentar(X) :- ('this is from an article in BYTE august 1987').


inv(0,1).
inv(1,0).

and(0 , 0 , 0 ).
and(0 , 1 , 0 ).
and(1 , 0 , 0 ).
and(1 , 1 , 1 ).
nand(0 , 0 , 1 ).
nand( 0 , 1 , 1 ).
nand( 1 , 0 , 1 ).
nand( 1 , 1 , 0 ).
xor(X , Y , Z ) :- nand(X , Y , A ),
                   nand(X , A , B ),
                   nand(Y , A , C ),
                   nand(B , C , Z ).

dff(1,0,1,1).
dff(1,0,0,0).
dff(0,0,1,1).
dff(0,0,0,0).
dff(1,1,1,1).
dff(1,1,0,1).
dff(0,1,1,0).
dff(0,1,0,0).

div(X , Q , Y ) :- inv(Q ,D ),
              dff(D , X , Q , Y ).

divide([] , S , [] ).
divide([P|Ps] , S, [Q|Qs] ) :- div(P , S , Q ) ,
                               divide(Ps , Q , Qs).
