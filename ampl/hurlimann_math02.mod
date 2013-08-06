/*
MODEL TWELVE ;
  VARIABLE X{1:12} [0,5] ;
  CONSTRAINT
    A: SUM{i={1:12}} X[i]     = 12 ; 
    B: X[1]+X[2]+X[3]+X[4]    = 5  ; 
    C: X[1]+X[5]+X[7]+X[9]    = 5  ; 
    D: X[4]+X[6]+X[8]+X[12]   = 5  ; 
    E: X[9]+X[10]+X[11]+X[12] = 5  ; 
  MINIMIZE any: X[2];
  WRITE 'There should be:\n%2d guards at position %2d\n':
    ROW{i={1:12}|X[i]} (X[i],i);
END


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/    

var X{1..12} >= 0 integer; # <=5;

s.t.
        A: sum{i in 1..12} X[i]     = 12;
        B: X[1]+X[2]+X[3]+X[4]    = 5; 
        C: X[1]+X[5]+X[7]+X[9]    = 5; 
        D: X[4]+X[6]+X[8]+X[12]   = 5; 
        E: X[9]+X[10]+X[11]+X[12] = 5; 

minimize any: X[2];

option solver cplex;
solve;

display X;

#for{i in 1..12} {
#        printf "There should be:\n%2d guards at position %2d\n", X[i],i;
#}
printf "%d %d %d %d\n", X[1],X[2], X[3], X[4];
printf "%d     %d\n", X[5],X[6];
printf "%d     %d\n", X[7],X[8];
printf "%d %d %d %d\n", X[9],X[10], X[11], X[12];
