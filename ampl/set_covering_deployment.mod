/*

Set covering deployment
(a.k.a. "roman domaination", "roman graph")

http://mathworld.wolfram.com/SetCoveringDeployment.html
"""
Set covering deployment (sometimes written "set-covering deployment" and abbreviated 
SCDP for "set covering deployment problem") seeks an optimal stationing of troops 
in a set of regions so that a relatively small number of troop units can control a 
large geographic region. ReVelle and Rosing (2000) first described this in a study of 
Emperor Constantine the Great's mobile field army placements to secure the Roman Empire. 
Set covering deployment can be mathematically formulated as a (0,1)-integer programming  problem.

To formulate the Roman domination problem, consider the eight provinces of the Constantinian 
Roman Empire illustrated above. Each region is represented as a white disk, and the red 
lines indicate region connections. Call a region secured if one or more field armies are 
stationed in that region, and call a region securable if a field army can be deployed to 
that area from an adjacent area. In addition, assume that a field army can only be deployed 
to an adjacent region if at least one army remains in the original region to provide 
logistical support. Also assume that each region contains at most two armies, as the 
number of available armies are limited and cannot be concentrated in any one region.
"""

In set covering deployment, the problem to be solved is to maximize the quantity
  sum{i in 1..n} (X[i] - Y[i])

subject to the constraints: 
 1)  {i in 1..n } X[i] > Y[i]

which guarantees that the first legion is stationed at a given vertex before a second can be,
 2) {i in 1..n} X[i] + sum{j in (vi, vj) in graph} Y[j] 

which guarantees that if v_i does not contain a field army, it has a neighbor with two field armies, and
 3) X_i,Y_i in 0,1 for all i,
   X[i] binary;
   Y[i] binary;

Interesting note at the end of the article:
"""
In the Season 4 opening episode "Trust Metric" (2007) of the television crime drama NUMB3RS, 
math genius Charlie Eppes uses set covering deployment as an analogy for optimizing the 
position of police officers in downtown Los Angeles in a search for escaped fugitives.
[See further: http://numb3rs.wolfram.com/401/]
"""

In Rubalcaba, R. R. "Fractional Domination, Fractional Packings, and Fractional Isomorphisms of Graphs." Ph.D. dissertation. Auburn, Alabama: Auburn University. May 13, 2005. http://webpages.uah.edu/~rubalcr/RUBALCABA.pdf.

Page 86 contains the following math program (translated to AMPL):
  minimize  sum{i in nodes} (X[i] + Y[i])
  s.t. 
      c1{i in nodes}: X[i] >= Y[i]; 
      c2{i in nodes}: (X[i] + sum{j in nodes: matrix[i,j] = 1} Y[j]) >= 1;      
      X och Y binary
Notera: here it's a minimization problem.

Rubalcaba gives the following result:
    britain: 1
    rome: 2
    asia minor: 1

Which is the same as this model.

:            X   Y    :=
alexandria   0   0
asia_minor   1   0
britain      1   0
byzantium    0   0
gaul         0   0
iberia       0   0
rome         1   1
tunis        0   0
;


Other solvers give other solutions:
  glpk: alexandria 2, iberia 2
  bonmin: alexandria 2, britain 2
  lpsolve: samma som bonmin
  cbc: alexandria 2, gaul 2
   

Here's the graph:

    britain     gaul
    britain     iberia

    gaul        iberia 
    gaul        britain
    gaul        rome

    iberia      britain
    iberia      gaul
    iberia      rome
    iberia      tunis
    
    rome        gaul
    rome        iberia
    rome        tunis 
    rome        byzantium
    rome        alexandria
  
    byzantium   rome
    byzantium   asia_minor
    byzantium   alexandria

    tunis       iberia
    tunis       rome
    tunis       alexandria

    alexandria  rome
    alexandria  tunis
    alexandria  byzantium
    alexandria  asia_minor

    asia_minor  alexandria
    asia_minor  byzantium


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set nodes;
param matrix{nodes, nodes} binary default 0;

var X{nodes} binary; # first army
var Y{nodes} binary; # reserve with _2_ armies

#maximize z:  sum{i in nodes} (X[i] - Y[i]);
minimize z:  sum{i in nodes} (X[i] + Y[i]);

# constraint 1: Det finns alltid en arme i en stad + ev backup. 
#               There is alway an army in a city (+ perhaps a backup)
#               X[i] > Y[i] för alla i
#               (It should be >=, and not >, shouldn't it?)
c1{i in nodes}: X[i] >= Y[i];

# constraint 2. There is always an reserve army nearby
#               X[i] + sum( (i,j) in vertices ) Y[j] > 1
c2{i in nodes}: (X[i] + sum{j in nodes: matrix[i,j] = 1} Y[j])  >= 1;


data;

# with self loops
set nodes := 'alexandria' 'asia_minor' 'britain' 'byzantium' 'gaul' 'iberia' 'rome' 'tunis';
param matrix: 'alexandria' 'asia_minor' 'britain' 'byzantium' 'gaul' 'iberia' 'rome' 'tunis' :=

'alexandria'       .   1   .   1   .   .   1   1
'asia_minor'       1   .   .   1   .   .   .   .
 'britain'         .   .   .   .   1   1   .   .
'byzantium'        1   1   .   .   .   .   1   .
    'gaul'         .   .   1   .   .   1   1   .
  'iberia'         .   .   1   .   1   .   1   1
    'rome'         1   .   .   1   1   1   .   1
   'tunis'         1   .   .   .   .   1   1   .
;


option solver cplex; # give Rubalcaba's solution
#option solver bonmin; # 2 in alexandria and 2 in britain
# option solver lpsolve; # same as bonmin
# option solver cbc;
solve;

display X, Y;

# expand;

