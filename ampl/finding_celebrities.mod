/*

  Finding celebrities problem in AMPL+CP.

  From Uwe Hoffmann
  "Finding celebrities at a party"
  http://www.codemanic.com/papers/celebs/celebs.pdf
  """
  Problem: Given a list of people at a party and for each person the list of
  people they know at the party, we want to find the celebrities at the party. 
  A celebrity is a person that everybody at the party knows but that 
  only knows other celebrities. At least one celebrity is present at the party.
  """
  (This paper also has an implementation in Scala.)
  
  Note: The original of this problem is 
    Richard Bird and Sharon Curtis: 
    "Functional pearls: Finding celebrities: A lesson in functional programming"
    J. Funct. Program., 16(1):13–20, 2006.
  but I (as well as Hoffmann) have not been able to access this paper.

  The problem from Hoffmann's paper is to find of who are the 
  celebrity/celebrities in this party graph:
    Adam  knows {Dan,Alice,Peter,Eva},
    Dan   knows {Adam,Alice,Peter},
    Eva   knows {Alice,Peter},
    Alice knows {Peter},
    Peter knows {Alice}
  
  Solution: the celebrities are Peter and Alice.

  Note: We assume that a person know him/herself, since it makes the
  calculations somewhat easier.

  Note2: I blogged about this problem (and a MiniZinc model) in 
  "Finding celebrities at a party"
  http://www.hakank.org/constraint_programming_blog/2010/01/finding_celebrities_at_a_party.html


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param graph{1..n, 1..n} default 0; # The party.


var x{1..n} binary; # 1: x[i] is a celebrity.
var num_celebrities >= 0 <= n integer;


#
# constraints
#

#  there is at least one celebrity
s.t. c1:  num_celebrities >= 1;

# all persons know the celebrities,
s.t. c2{i in 1..n}:
     # This don't work
     # if x[i] = 1 then (n = count{j in 1..n} (graph[j,i] = 1))

     # Both these works:
     x[i] = 1 ==> (n = count{j in 1..n} (graph[j,i] = 1))
     # (x[i] = 1) ==> (n = sum{j in 1..n} (graph[j,i]))
;

# and the celebrities only know celebrities (including him-/herself)
s.t. c3{i in 1..n}:
     # This don't work
     # if x[i] = 1 then num_celebrities = count{j in 1..n} (graph[i,j] = 1)

     # Both these works:
     x[i] = 1 ==> (num_celebrities = count{j in 1..n} (graph[i,j] = 1))
     # x[i] = 1 ==> (num_celebrities = sum{j in 1..n} graph[i,j])
;

s.t. c4: num_celebrities = sum{i in 1..n} x[i];

# data finding_celebrities1.dat;
# data finding_celebrities2.dat;
data finding_celebrities3.dat;
# data finding_celebrities4.dat;

# data;


# The party graph of the example above:
#
#  Adam  knows {Dan,Alice,Peter,Eva},  {2,3,4,5}
#  Dan   knows {Adam,Alice,Peter},     {1,4,5}
#  Eva   knows {Alice,Peter},          {4,5}
#  Alice knows {Peter},                {5}
#  Peter knows {Alice}                 {4}
#
# Also, we assume that a celebrity knowns him-/herself.
# param n := 5;
# param graph: 1 2 3 4 5 :=
#   # 1 2 3 4 5
# 1   1 1 1 1 1
# 2   1 1 . 1 1
# 3   . . 1 1 1
# 4   . . . 1 1
# 5   . . . 1 1
# ;

## Generate a random problem instance. Though it's very small chance
## of hitting a graph with some celebrity.
# param n := 15;
# for {i in 1..n} {
#    for{j in 1..n} {
#       let graph[i,j] := Irand224() mod 2;
#       printf "%d ", graph[i,j];
#    }
#    printf "\n";
# }
# printf "\n";

# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1 printproblem=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

# option solver cplex;

solve;

display x;
display num_celebrities;

printf "The celebrities are:";
for {i in 1..n} {
  if x[i] = 1 then printf "%2d ", i;
}
printf "\n";