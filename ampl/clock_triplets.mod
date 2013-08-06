/*

  Clock Triplet problem AMPL+CP.

  Problem formulation
  http://www.f1compiler.com/samples/Dean%20Clark%27s%20Problem.f1.html
  """
  Dean Clark's Problem (Clock Triplets Problem)

  The problem was originally posed by Dean Clark and then presented
  to a larger audience by Martin Gardner. 

  The problem was discussed in Dr. Dobbs's Journal, May 2004 in an article 
  by Timothy Rolfe. According to the article, in his August 1986 column for 
  Isaac Asimov's Science Fiction Magazine, Martin Gardner presented this problem:
  
    Now for a curious little combinatorial puzzle involving the twelve
    numbers on the face of a clock. Can you rearrange the numbers (keeping
    them in a circle) so no triplet of adjacent numbers has a sum higher 
    than 21? This is the smallest value that the highest sum of a triplet
    can have.
  """

  There are 261 different solutions to this problem.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var x{0..11} >= 1 <= 12 integer;
var triplet_sum >= 0 <= 100 integer;

## check if 21 really is the highest value
# minimize obj: triplet_sum; 

#
# constraints
#
s.t. c1: triplet_sum <= 21;
s.t. c2: alldiff{i in 0..11} x[i];
s.t. c3: x[0] = 12 and x[1] > x[11];

s.t. c4{i in 2..11}:
   x[i] + x[i-1] + x[i-2] <= triplet_sum
;
s.t. c5:
    # and around the corners
    x[10] + x[11] + x[0]  <= triplet_sum and 
    x[11] + x[0]  + x[1]  <= triplet_sum
;

data;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x, triplet_sum;

for{i in 0..11} {
  printf "%d, ", x[i];
}
printf "\n";