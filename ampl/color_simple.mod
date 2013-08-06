/*
  A simple version of map coloring.

  From 
  Katta G. Murty: "Optimization Models for Decision Making", sid 344f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf

cplex:
Belgium : 2
Denmark : 1
France : 3
Germany : 4
Netherlands : 1
Luxembourg : 1
France (3) Belgium (2)
France (3) Luxembourg (1)
France (3) Germany (4)
Luxembourg (1) Germany (4)
Luxembourg (1) Belgium (2)
Belgium (2) Netherlands (1)
Belgium (2) Germany (4)
Germany (4) Netherlands (1)
Germany (4) Denmark (1)

glpk:
Belgium : 3
Denmark : 1
France : 2
Germany : 4
Netherlands : 1
Luxembourg : 1
France (2) Belgium (3)
France (2) Luxembourg (1)
France (2) Germany (4)
Luxembourg (1) Germany (4)
Luxembourg (1) Belgium (3)
Belgium (3) Netherlands (1)
Belgium (3) Germany (4)
Germany (4) Netherlands (1)
Germany (4) Denmark (1)


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/


*/

# Added some countries...
set countries := {"Belgium", "Denmark", "France", "Germany", "Netherlands", "Luxembourg"}; #, "Sweden", "Norway", "Finland", "Spain", "Portugal", "Poland", "Switzerland", "Austria", "Italy" };
param n := 4;
set colors := 1..n;# {"blue", "red", "yellow", "gray"};
set graph within {countries cross countries};
var numColors integer >= 0 <= n;

# x: which color
var x{countries} integer >= 1 <= n;

# indicator
var y{graph, 1..2} binary;

# minimize the number of colors
minimize z:
        numColors;
        # sum{i in countries} x[i];

s.t. c1{ (i,j) in graph}:
        x[i] - x[j] - 1 + 2*n*y[i,j,1] >= 0;

s.t. c2{ (i,j) in graph}:
       -x[i] + x[j] - 1 + 2*n*y[i,j,2] >= 0;

s.t. c3{ (i,j) in graph}:
        y[i,j,1] + y[i,j,2] = 1;

s.t. c4{i in countries}: 
        x[i] <= numColors;

#/*      
data;

set graph := 
        "France" "Belgium"
        "France" "Luxembourg"
        "France" "Germany"
#        "France" "Spain"
#        "Spain" "Portugal"
        "Luxembourg" "Germany"
        "Luxembourg" "Belgium"
        "Belgium" "Netherlands"
        "Belgium" "Germany"
        "Germany" "Netherlands"
        "Germany" "Denmark"
#        "Germany" "Poland"
#        "Germany" "Austria"
#        "Denmark" "Sweden"
#        "Sweden" "Norway"
#        "Sweden" "Finland"
#        "Sweden" "Poland"
#        "Finland" "Norway"
#        "France" "Switzerland"
#        "France" "Italy"
#        "Italy" "Switzerland"
#        "Italy" "Austria"
;
#*/

option solver cplex;

solve;

# display Colors;
# display y;
# display x;

for{ i in countries} {
   printf "%s : %d\n", i, x[i];
}

printf "\n";

for{ (i,j) in graph} {
   printf "%s (%d) %s (%d) ", i, x[i], j, x[j];
   if x[i] = x[j] then {
     printf " SAME!!!";
   } 
   printf "\n";

}


/*      
data;

set graph := 
        "France" "Belgium"
        "France" "Luxembourg"
        "France" "Germany"
        "France" "Spain"
        "Spain" "Portugal"
        "Luxembourg" "Germany"
        "Luxembourg" "Belgium"
        "Belgium" "Netherlands"
        "Belgium" "Germany"
        "Germany" "Netherlands"
        "Germany" "Denmark"
        "Germany" "Poland"
        "Germany" "Austria"
        "Denmark" "Sweden"
        "Sweden" "Norway"
        "Sweden" "Finland"
        "Sweden" "Poland"
        "Finland" "Norway"
        "France" "Switzerland"
        "France" "Italy"
        "Italy" "Switzerland"
        "Italy" "Austria"
;
*/
