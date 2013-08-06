/*

   Marathon puzzle in AMPL+CP.

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


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set pos = 1..6;
set runners = {"Dominique","Ignace","Naren","Olivier","Philippe","Pascal"};

param n := 6;
var x{runners} >= 1 <= 6 integer; 
var places{runners} >= 1 <= 6 integer;

#
# constraints
#
# One runner per position
s.t. c0: alldiff{i in runners} x[i];

# a: Olivier not last
s.t. a: x["Olivier"] < 6;

# b: Dominique, Pascal and Ignace before Naren and Olivier
s.t. b: 
  x["Dominique"] < x["Naren"] and
  x["Dominique"] < x["Olivier"] and
  x["Pascal"]    < x["Naren"] and
  x["Pascal"]    < x["Olivier"] and
  x["Ignace"]    < x["Naren"] and
  x["Ignace"]    < x["Olivier"]
;

# c: Dominique better than third
# subject to c: x["Dominique"] < 3;
subject to c: x["Dominique"] <= 2;


# d: Philippe is among the first four
subject to d: x["Philippe"] <= 4;

# e: Ignace neither second nor third
subject to e: x["Ignace"] != 2 and +x["Ignace"] != 3;

# f: Pascal three places earlier than Naren
subject to f: x["Pascal"] + 3 = x["Naren"];

# g: Neither Ignace nor Dominique on fourth position
s.t. g:
   x["Ignace"] != 4 and 
   x["Dominique"] != 4;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=60";

# option solver ilogcp;

solve;

for {r in runners} {
    print r, x[r];
};





