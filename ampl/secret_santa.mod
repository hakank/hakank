/*

  Secret Santa problem in AMPL+CP.
  
  From Ruby Quiz Secret Santa
  http://www.rubyquiz.com/quiz2.html
  """
  Honoring a long standing tradition started by my wife's dad, my friends 
  all play a Secret Santa game around Christmas time. We draw names and 
  spend a week sneaking that person gifts and clues to our identity. On the 
  last night of the game, we get together, have dinner, share stories, and, 
  most importantly, try to guess who our Secret Santa was. It's a crazily 
  fun way to enjoy each other's company during the holidays.
  
  To choose Santas, we use to draw names out of a hat. This system was 
  tedious, prone to many "Wait, I got myself..." problems. This year, we 
  made a change to the rules that further complicated picking and we knew 
  the hat draw would not stand up to the challenge. Naturally, to solve 
  this problem, I scripted the process. Since that turned out to be more 
  interesting than I had expected, I decided to share.
  
  This weeks Ruby Quiz is to implement a Secret Santa selection script.
  
  Your script will be fed a list of names on STDIN. 
  ...
  Your script should then choose a Secret Santa for every name in the list. 
  Obviously, a person cannot be their own Secret Santa. In addition, my friends 
  no longer allow people in the same family to be Santas for each other and your 
  script should take this into account.
  """

  Comment: Well, this model skips the file input and mail parts. We 
           assume that the friends are identified with a number from 1..n,
           and the families is identified with a number 1..num_families. 

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param num_families;
param family{1..n};

var x{1..n} >= 1 <= n integer;


#
# constraints
#
# Everyone gives and receives a Secret Santa
s.t. c1:  alldiff{i in 1..n} x[i]; 

# Can't be one own's Secret Santa (no fix point)
s.t. c2{i in 1..n}: x[i] != i;

# No Secret Santa to a person in the same family
# MiniZinc code:
#     forall(i in index_set(x)) (
#        family[i] != family[x[i]]
#     )
s.t. c3{i in 1..n}:
     exists{j in 1..n}
       j = x[i] and
       family[i] != family[j]
;

data;

param n := 12;
param num_families := 4;

param family :=
 1  1
 2  1
 3  1
 4  1
 5  2
 6  3
 7  3
 8  3
 9  3
10  3
11  4 
12  4
;

# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display family, x;

