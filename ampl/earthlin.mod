/*
   The third response is the true one.	 

    Xpress-Mosel Model

    model 'earthlin'

    ! Description  : Earthlings
    ! Source       : Poniachik, J. & L., (1998), Hard-to-solve Brainteasers, Sterling  
    ! Date written : Xpress-MP 2/12/99, Mosel 19/4/03
    ! Written by   : M J Chlond 

    uses 'mmxprs'

Xpress:
0 0 1 
1 0 0 
0 1 0 

0 0 1 
0 1 0 
1 0 0 


cplex
x:
0 0 1
1 0 0
0 1 0

y:
0 0 1
0 1 0
1 0 0


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/  

param team  := 3;   # 1 = Zaire, 2 = Uruguay, 3 = Spain
param place := 3;   # 1st, 2nd or 3rd 
param type  := 3;   # 1 = truth-teller, 2 = alternator, 3 = liar
param state := 3;   # statements 1 = x(1,1)+x(2,2)+x(3,3)=3
                    #            2 = x(1,1)+x(3,2)+x(2,3)=3
                    #            3 = x(2,1)+x(3,2)+x(1,3)=3 
set T := 1..team;
set P := 1..place;
set E := 1..type;
set S := 1..state;
var x{T,P} binary; # x(i,j) = 1 if team i in place j , 0 otherwise 
var y{S,E} binary; # y(k,l) = 1 if statement k made by type l
var d{S} integer <= 3;  # d(k) = number of truths in statement k

minimize any: x[1,1];

# each place one team
s.t. tecon{j in P}:
        sum{i in T} x[i,j] = 1;

# each team one place
s.t. plcon{i in T}:
        sum{j in P} x[i,j] = 1;

# each type makes one statement
s.t. tycon{k in E}:
        sum{l in S} y[k,l] = 1;

# each statement made by one type
s.t. stcon{l in S}:
        sum{k in E} y[k,l] = 1;

# d[i] = number of truths in statement i
s.t. setd1: x[1,1]+x[2,2]+x[3,3] = d[1];
s.t. setd2: x[1,1]+x[3,2]+x[2,3] = d[2];
s.t. setd3: x[2,1]+x[3,2]+x[1,3] = d[3];

s.t. sta{k in S}:
# if statement k made by truthteller (i.e. d[k]=3 ] then y[k,1] = 1, else 0
        d[k] - 3*y[k,1] >= 0;
s.t. stb{k in S}:
        d[k] - 3*y[k,1] <= 2;

  # if statement k made by liar (i.e. d[k]=0 ] then y[k,3] = 1, else 0
s.t. sla{k in S}:
        d[k] + 3*y[k,3] <= 3;
s.t. slb{k in S}: d[k] + y[k,3] >= 1;
  
# assertion 1 and 3 either both true or both false for all statements
s.t.  fta: x[1,1]=x[3,3];
s.t.  ftb: x[1,1]=x[2,3];
s.t.  ftc: x[2,1]=x[1,3];

option solver cplex;

solve;

display x;
display y;
display d;
  

printf "x:\n";
for{i in T} {
    for{j in T} {
        printf "%d ", x[i,j];
    }
    printf "\n";
}


printf "\ny:\n";
for{l in S} {
    for{k in E} {
        printf "%d ", y[l,k];
    }
    printf "\n";
}


printf "\nd (statements):\n";
for{i in S} {
   printf "%d %d ", i, d[i]; 
   if d[i] = 3 then {
     printf "<--";
   }
   printf "\n";
}
printf "\n";

