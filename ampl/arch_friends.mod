/*

  Arch friends puzzle (Dell Logic Puzzles) in AMPL+CP.

  Problem formulation from 
  http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
  """
  Title: Arch Friends
  Author: Mark T. Zegarelli
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Harriet, upon returning from the mall, is happily describing her four shoe 
  purchases to her friend Aurora. Aurora just loves the four different kinds 
  of shoes that Harriet bought (ecru espadrilles, fuchsia flats, purple pumps, 
  and suede sandals), but Harriet can't recall at which different store (Foot 
  Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) she got each pair. 
  Can you help these two figure out the order in which Harriet bought each 
  pair of shoes, and where she bought each?

  1. Harriet bought fuchsia flats at Heels in a Handcart.
  2. The store she visited just after buying her purple pumps was not Tootsies.
  3. The Foot Farm was Harriet's second stop.
  4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
  
  Determine: Order - Shoes - Store 
  """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
set shoes_set;
set shops_set;

var shoes{shoes_set} >= 1 <= n integer;
var shops{shops_set} >= 1 <= n integer;


#
# constraints
#
s.t. c0a: alldiff{i in shoes_set} shoes[i];
s.t. c0b: alldiff{i in shops_set} shops[i];

# 1. Harriet bought fuchsia flats at Heels in a Handcart.
s.t. c1:  shoes['fuchsia_flats'] = shops['Heels_in_a_Handcart'];

#  2. The store she visited just after buying her purple pumps was not Tootsies.
s.t. c2: shoes['purple_pumps'] + 1 != shops['Tootsies'];

# 3. The Foot Farm was Harriet's second stop.
s.t. c3: shops['Foot_Farm'] = 2;

# 4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
s.t. c4: shops['The_Shoe_Palace'] + 2 = shoes['suede_sandals'];



data;

param n := 4;
set shoes_set := ecru_espadrilles fuchsia_flats purple_pumps  suede_sandals;
set shops_set :=  Foot_Farm Heels_in_a_Handcart The_Shoe_Palace Tootsies;

option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";


solve;

display shoes, shops;

for{i in 1..n} {
  printf "%d: ", i; 
  for{s in shoes_set} { if shoes[s] = i then printf "%s ", s };
  for{s in shops_set} { if shops[s] = i then printf "%s ", s };
  printf "\n";
}

printf "\n";