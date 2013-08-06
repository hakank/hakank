/* 
   Game theory, from http://www.ms.uky.edu/~mathecon/ma416

   The data instance is from 
   Vanderbei "Linear Programming - Foundations - and Extensions", sid 174

Solution games.dat (page 180)
; 40/102
        ~0.39215686274509803922
; 36/102
        ~0.35294117647058823529
; 26/102
        ~0.25490196078431372549


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/


*/
set I;
set J;
param A {I,J};      # payoff matrix
var X{J} >= 0;    # probability of strategy j for column player
var w;

maximize game:
        w;
subject to constraints {i in I}:
        sum {j in J} A[i,j]*X[j]-w >=0;

subject to probability:
        sum {j in J} X[j]=1;


 

data games.dat;

solve;

display _obj;
display _varname, _var;
display _conname, _con;

 