/*

  Who owns the Zebra problem in AMPL+CP.

  Problem formulation from a MiniZinc model by Peter Stuckey, September 2006:
  """
  This is a puzzle which has been circulating the net. There are a couple
  different versions out there which try to be a little more politically
  correct but are essentially the same problem.    
  	1. There are five houses, each of a different color and inhabited by
	   men of different nationalities, with different pets, drinks,
	   and cigarettes.
  	2. The Englishman lives in the red house.
  	3. The Spaniard owns the dog.
  	4. Coffee is drunk in the green house.
  	5. The Ukrainian drinks tea.
  	6. The green house is immediately to the right of the ivory house.
  	7. The Old Gold smoker owns snails.
  	8. Kools are smoked in the yellow house.
  	9. Milk is drunk in the middle house.
  	10. The Norwegian lives in the first house on the left.
  	11. The man who smokes Chesterfields lives in the house next to the
	    man with the fox.
  	12. Kools are smoked in the house next to the house where the horse is
	    kept.
  	13. The Lucky Strike smoker drinks orange juice.
  	14. The Japanese smoke Parliaments.
  	15. The Norwegian lives next to the blue house.
  NOW, who drinks water? And who owns the zebra?
  """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n := 5;

set nationalities;
set colors;
set animals;
set drinks;
set cigarettes;

var na{nationalities} >= 1 <= n integer;
var co{colors}        >= 1 <= n integer;
var an{animals}       >= 1 <= n integer;
var dr{drinks}        >= 1 <= n integer;
var ci{cigarettes}    >= 1 <= n integer;

#
# constraints
#

# Note: Cannot combine these into one constraint because ampl 
#       complains about nested alldiff...
s.t. c1a: alldiff{i in nationalities} na[i];
s.t. c1b: alldiff{i in colors} co[i];
s.t. c1c: alldiff{i in animals} an[i];
s.t. c1d: alldiff{i in drinks} dr[i];
s.t. c1e: alldiff{i in cigarettes} ci[i];

s.t. c2:
  	na["English"]     = co["Red"]            and
	na["Spanish"]     = an["Dog"]            and
	dr["Coffee"]      = co["Green"]          and
        na["Ukrainian"]   = dr["Tea"]            and
        co["Green"]       = co["Ivory"] + 1      and
        ci["OldGold"]     = an["Snails"]         and
        ci["Kools"]       = co["Yellow"]         and
        dr["Milk"]        = 3                    and
        na["Norwegian"]   = 1                    and
        abs(ci["Chesterfields"] - an["Fox"]) = 1 and
        abs(ci["Kools"] - an["Horse"])       = 1 and
        ci["LuckyStrike"] = dr["OrangeJuice"]    and
        na["Japanese"]    = ci["Parliaments"]    and
        abs(na["Norwegian"] - co["Blue"])    = 1
;


data;

set nationalities = English Spanish Ukrainian Norwegian Japanese;
set colors        = Red Green Ivory Yellow Blue;
set animals       = Dog Fox Horse Zebra Snails;
set drinks        = Coffee Tea Milk OrangeJuice Water;
set cigarettes    = OldGold Kools Chesterfields LuckyStrike Parliaments;

option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display na,co,an,dr,ci;


for{i in 1..n} {
   printf "\nHouse %d: ", i;
   for{j in nationalities} { if na[j] = i then { printf "%-14s ", j} };
   for{j in colors}        { if co[j] = i then { printf "%-14s ", j} };
   for{j in animals}       { if an[j] = i then { printf "%-14s ", j} };
   for{j in drinks}        { if dr[j] = i then { printf "%-14s ", j} };
   for{j in cigarettes}    { if ci[j] = i then { printf "%-14s ", j} };
}
printf "\n";

# NOW, the answers of the questions.
for{j in nationalities} { 
  if na[j] = dr["Water"] then { printf "The %s drinks Water (house %d)\n", j, dr["Water"]; }
  if na[j] = an["Zebra"] then { printf "The %s owns the Zebra. House %d\n", j, an["Zebra"]; }
}
