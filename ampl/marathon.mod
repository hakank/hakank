/*

   Marathon puzzle

   Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
   have arrived as the first six at the Paris marathon.
   Reconstruct their arrival order from the following
   information:
   a) Olivier has not arrived last
   b) Dominique, Pascal and Ignace have arrived before Naren
      and Olivier
   c) Dominique who was third last year has improved this year.
   d) Philippe is among the first four.
   e) Ignace has arrived neither in second nor third position.
   f) Pascal has beaten Naren by three positions.
   g) Neither Ignace nor Dominique are on the fourth position.

   (c) 2002 Dash Associates
  author: S. Heipcke, Mar. 2002

Solution determined by presolve;
objective MIN = 0.
arrive [*,*]
:           1   2   3   4   5   6    :=
Dominique   0   1   0   0   0   0
Ignace      1   0   0   0   0   0
Naren       0   0   0   0   0   1
Olivier     0   0   0   0   1   0
Pascal      0   0   1   0   0   0
Philippe    0   0   0   1   0   0
;

Dominique 2
Ignace 1
Naren 6
Olivier 5
Philippe 4
Pascal 3

Sorted:

Ignace 1
Dominique 2
Pascal 3
Philippe 4
Olivier 5
Naren 6


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set POS = 1..6; #                          ! Arrival positions
set RUNNERS = {"Dominique","Ignace","Naren","Olivier","Philippe","Pascal"};

var arrive{RUNNERS,POS} binary; # 1 if runner is p-th, 0 otherwise


# One runner per position
s.t. onerunner{p in POS}: 
     sum{r in RUNNERS} arrive[r,p] = 1;

# One position per runner
s.t. oneposition{r in RUNNERS}:
      sum{p in POS} arrive[r,p] = 1;

# a: Olivier not last
s.t. a:
 arrive["Olivier",6] = 0;

# b: Dominique, Pascal and Ignace before Naren and Olivier
subject to b:
 sum{p in 5..6} (arrive["Dominique",p]+arrive["Pascal",p]+arrive["Ignace",p]) =
   0;

subject to b2: 
         sum{p in 1..3} (arrive["Naren",p]+arrive["Olivier",p]) = 0;

# c: Dominique better than third
subject to c:
 arrive["Dominique",1]+arrive["Dominique",2] = 1;

# d: Philippe is among the first four
subject to d:
 sum{p in 1..4} arrive["Philippe",p] = 1;

# e: Ignace neither second nor third
subject to e:
 arrive["Ignace",2]+arrive["Ignace",3] = 0;

# f: Pascal three places earlier than Naren
subject to f:
 sum{p in 4..6} arrive["Pascal",p] = 0;

subject to f2:
 sum{p in 1..3} arrive["Naren",p] = 0;

# g: Neither Ignace nor Dominique on fourth position
subject to g:
 arrive["Ignace",4]+arrive["Dominique",4] = 0;

# forall{p in POS, r in RUNNERS} arrive[r,p] is_binary

# ! Solve the problem: no objective
minimize MIN:
          0;

option solver cplex;
solve;

display arrive;

for {r in RUNNERS} {
    print r, sum{p in POS} p*arrive[r,p];         
};
# ! Solution printing
#  forall{r in RUNNERS} writeln{r,": ", getsol{sum{p in POS}p*arrive[r,p]} }



