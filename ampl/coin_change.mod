/*
  
   coin change

   http://www.cs.rhul.ac.uk/home/green/publications/thesis/MJGreen_MScThesis.ps
   Martin Green: Implementing 'Constraint Satisfaction Problems' in 
   'Optimization Programming Language'
   Page 4ff
   """
   Consider a vending machine which ha to give the customer change of up to 
   99 pence in the smallest number of coins possible.
   """


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/


*/


param vT  integer >=0 <= 99 default 39; # Total change to give
var v50 integer >=0 <= 1; # 50 pence
var v20 integer >=0 <= 4; # 20 pence
var v10 integer >=0 <= 9; # 10 pence
var v5  integer >=0 <= 19; # 5 pence
var v2  integer >=0 <= 49; # 2 pence
var v1  integer >=0 <= 99; # 1 pence

minimize z:
        v50 + v20 + v10 + v5 + v2 + v1;

subject to change:
        vT = 50*v50 + 20*v20 + 10*v10 + 5*v5 + 2*v2 + v1;

# Some heuristic constraints
# h1: v20 <= 4;
# h2: v10 <= 1;
# h3: v5 <= 1;
# h4: v2 <= 2;
# h5: v1 <= 1;
# h6: v20 + v10 <= 2;
# h7: v2 + v1 <= 2;




# data;

# option solver bonmin;
# option solver cplex;
option solver gecode;

# let vT := 39;
for{i in 1..99} {
   let vT := i;
   solve;
# display _varname, _var;
# display v50,v20, v10,v5,v2,v1;
printf "%d*50 + %d*20 + %d*10 + %d*5 + %d*2 + %d*1 = %d (amount:%d)\n", v50,v20,v10,v5,v2,v1,vT,z;
}

display z;

end;
