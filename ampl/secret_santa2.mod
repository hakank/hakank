/*

  Secret Santa problem II in AMPL+CP.

  From Maple Primes: "Secret Santa Graph Theory"
  http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
  """
  Every year my extended family does a "secret santa" gift exchange. 
  Each person draws another person at random and then gets a gift for 
  them. At first, none of my siblings were married, and so the draw was 
  completely random. Then, as people got married, we added the restriction 
  that spouses should not draw each others names. This restriction meant 
  that we moved from using slips of paper on a hat to using a simple 
  computer program to choose names. Then people began to complain when 
  they would get the same person two years in a row, so the program was 
  modified to keep some history and avoid giving anyone a name in their 
  recent history. This year, not everyone was participating, and so after 
  removing names, and limiting the number of exclusions to four per person, 
  I had data something like this:
  
  Name: Spouse, Recent Picks
  
  Noah: Ava. Ella, Evan, Ryan, John
  Ava: Noah, Evan, Mia, John, Ryan
  Ryan: Mia, Ella, Ava, Lily, Evan
  Mia: Ryan, Ava, Ella, Lily, Evan
  Ella: John, Lily, Evan, Mia, Ava
  John: Ella, Noah, Lily, Ryan, Ava
  Lily: Evan, John, Mia, Ava, Ella
  Evan: Lily, Mia, John, Ryan, Noah
  """
  
  Note: I interpret this as the following three constraints:
    1) One cannot be a Secret Santa of one's spouse
    2) One cannot be a Secret Santa for somebody two years in a row
    3) Optimization: maximize the time since the last time 

  This model also handle single persons, something the original
  problem don't mention.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n; # number of persons
param spouses{1..n}; # 0 for no spouse
param rounds{1..n, 1..n} default n+1;

# decision variables
var santas{1..n} >= 1 <= n integer;
var santa_distance{1..n} >= 1 <= n+1 integer;
var z >= 0 integer;

maximize obj: z;

#
# constraints
#
s.t. c1: alldiff{i in 1..n} santas[i];

# no Santa for a spouses
s.t. c2a{i in 1..n}: santas[i] != i;

s.t. c2b{i in 1..n: spouses[i] > 0}: santas[i] != spouses[i];


# optimize "distance" to earlier rounds:
s.t. c3{i in 1..n}: 
     exists{j in 1..n}
       j = santas[i] and    
       santa_distance[i] = rounds[i,j]
;

# cannot be a Secret Santa for the same person two years in a row.
s.t. c4{i in 1..n}:
   exists{j in 1..n} 
     rounds[i,j] = 1 and
     santas[i] != j
;

s.t. c5: z = sum{i in 1..n} santa_distance[i];



data;

param n := 9;

# Noah = 1
# Ava  = 2
# Ryan = 3
# Mia  = 4
# Ella = 5
# John = 6
# Lily = 7
# Evan = 8
# Single = 9 # not in the original problem
param spouses := 
  1  2  # Noa <-> Ava
  2  1  # Ava <-> Noa
  3  4  # Ryan <-> Mia
  4  3  # Mia <-> Ryan
  5  6  # Ella <-> John
  6  5  # John <-> Ella
  7  8  # Lily <-> Evan
  8  7  # Evan <-> Lily
  9  0  # Single no spouse
;

#
# rounds with a single person (fake data for that person)
#
# "." means that no earlier Santa.
#
# Note: Ryan and Mia has the same recipient for years 3 and 4,
#       and Ella and John has for year 4. 
#       This seems to be caused by modification of 
#       original data.
#
param rounds: 1 2 3 4 5 6 7 8 9 :=
 # N  A  R  M  El J  L  Ev S
1  0  .  3  .  1  4  .  2  2  # Noah
2  .  0  4  2  .  3  .  1  1  # Ava 
3  .  2  0  .  1  .  3  4  4  # Ryan
4  .  1  .  0  2  .  3  4  3  # Mia 
5  .  4  .  3  0  .  1  2  .  # Ella
6  1  4  3  .  .  0  2  .  .  # John
7  .  3  .  2  4  1  0  .  .  # Lily
8  4  .  3  1  .  2  .  0  .  # Evan
9  1  2  3  4  .  2  .  .  0  # Single
;

option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display santas, santa_distance;
display z;

