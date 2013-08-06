/*

  Cookie Bake Off problem (PuzzlOr) in MiniZinc.
  
  http://puzzlor.editme.com/Cookiebakeoff
  """
  Historically, baking has been considered an art rather than a science.  
  As a contestant in a cookie bake-off, your attempts to create the perfect cookie have 
  failed.  For your final attempt, you decide to use your analytical skills to analyze 
  your past attempts to see if you can find the recipe for the perfect cookie.
  Table 1 shows your past 15 attempted recipes, each with varying amounts of 
  Sugar, Flour, and Butter, and a corresponding judge’s score for each recipe.  
  The judge’s scores range from 0 (worst) to 100 (best).  None of your recipes so 
  far have achieved the coveted perfect score of 100.  All recipes sum up to 
  8 cups total of the three ingredients.
  
  Batch Sugar 	Flour 	Butter 	Score
  1 	3 	4 	1 	70
  2 	1 	4 	3 	95
  3 	2 	2 	4 	45
  4 	2 	1 	5 	20
  5 	2 	3 	3 	85
  6 	3 	2 	3 	55
  7 	2 	5 	1 	80
  8 	1 	1 	6 	15
  9 	1 	5 	2 	90
  10 	3 	3 	2 	75
  11 	4 	2 	2 	40
  12 	1 	3 	4 	65
  13 	1 	6 	1 	60
  14 	4 	1 	3 	25
  15 	4 	3 	1 	50
  
  Table 1
  
  Question:  Given your past recipes and scores, which recipe below has the best chance 
  of achieving a perfect score of 100?
     a.) 1 cup sugar; 2 cups flour; 5 cups butter
     b.) 2 cups sugar; 4 cups flour; 2 cup butter
     c.) 3 cups sugar; 1 cup flour; 4 cups butter
  
  Send your answer to puzzlor@gmail.com by February 15th, 2013. 
  """

*/

param n;
param scores{i in 1..n, 1..4};
param scores_test{i in 1..3, 1..3};

# IP version
var x{1..3} integer >= 1 <= 1000;
var diffs{1..n} integer >= -1000 <= 1000;

# non-IP
# var x{1..3} >= 1 <= 1000;
# var diffs{1..n}  >= -1000 <= 1000;


data;
param n := 15;

param scores: 1 2 3 4 :=
 1  3 4 1 70
 2  1 4 3 95 
 3  2 2 4 45 
 4  2 1 5 20 
 5  2 3 3 85 
 6  3 2 3 55 
 7  2 5 1 80 
 8  1 1 6 15 
 9  1 5 2 90 
10  3 3 2 75 
11  4 2 2 40 
12  1 3 4 65 
13  1 6 1 60 
14  4 1 3 25 
15  4 3 1 50
;

param scores_test: 1 2 3 :=
1  1 2 5
2  2 4 2
3  3 1 4
;

minimize z: 
   sum{i in 1..n} abs(diffs[i]);

s.t. c1{i in 1..n}: scores[i,4]=sum{j in 1..3} (x[j]*scores[i,j])+diffs[i];


# option solver cplex;
#option solver lpsolve;
# option solver cbc;
option solver gecode; # Wow, Gecode can solve this (though only the IP variant)
# option solver ipopt;
# option solver bonmin;
# option solver minos;


solve;

display x;
display diffs;

printf "Test scores:\n";
for{t in 1..3} {
  printf "Test %d: %d\n", t, (sum{i in 1..3} scores_test[t,i]*x[i]);
}

# for{a in 0..10, b in 0..10, c in 0..10} {
#   printf "Test %d %d %d: %d \n", a,b,c, a*x[1]+b*x[2]+c*x[3];
# }