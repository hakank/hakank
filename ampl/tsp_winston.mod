/*
  Wed Jan  2 18:40:27 2008/hakank@bonetmail.com

  Winston OR, sid 530ff, sid 536f TSP
  Baserad på Lingo-programmet TSP.lng (sid 537, saknas på disken)
  Bokens lösning:
"""
Joe should travel from Gary to Sounth Bend, from South Bend to Fort Wayne, from
Fort Wayne to Terre Haute, from Terre Haute to Evansville, and from Evansville
to Gary. Total distance: 668.
"""
dvs 
  Gary        -> South Bend
  South Bend  -> Fort Wayne
  Fort Wayne  -> Terre Haute
  Terre Haute -> Evansville
  Evansville  -> Gary


cplex, lpsolve:
1 (Gary) -> 5 (South Bend)
2 (Fort Wayne) -> 4 (Terre Haute)
3 (Evansville) -> 1 (Gary)
4 (Terre Haute) -> 3 (Evansville)
5 (South Bend) -> 2 (Fort Wayne)


glpk with --intopt gives the same result
Without --intopt (as with bonmin), the reversed result:
1 (Gary) -> 3 (Evansville)
2 (Fort Wayne) -> 5 (South Bend)
3 (Evansville) -> 4 (Terre Haute)
4 (Terre Haute) -> 2 (Fort Wayne)
5 (South Bend) -> 1 (Gary)


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set city;
var U{city} >= 0 integer;
param DIST{city, city} >= 0;
var x{city,city} binary;
param N := card(city);
param names{city} symbolic;

minimize z:
        sum{i in city, j in city} DIST[i,j]*x[i,j];

subject to 
        # exactly one outgoing
        c1{k in city}: sum{i in city} x[i,k] = 1;

        # exactly one incoming
        c2{k in city}: sum{j in city} x[k,j] = 1;

        # no subtours
        c3{k in city, j in city: j > 1 and k > 1}:  
           U[j] - U[k] + N*x[j,k] <= N-1;
 
data;

param: city: names := 
        1 "Gary"
        2 "Fort Wayne"
        3 "Evansville"
        4 "Terre Haute"
        5 "South Bend"
;


param DIST: 1 2 3 4 5 :=
        1 50000 132 217 164 58
        2 132 50000 290 201 79
        3 217 290 50000 113 303
        4 164 201 113 50000 196
        5 58 79 303 196 50000
;

# option cplex_options "sensitivity writeprob=xxx.lp";
option solver cplex;
# option solver minos;
# option solver cbc;
# option solver snopt;
# option solver gjh;
# option solver ipopt;
# option solver bonmin;
# option solver lpsolve;
# option solver donlp2;
# option solver loqo;

solve;

display z;
display x;
display U;

for{i in city} {
  for{j in city: x[i,j] >= 0.02} {
     printf "%d (%s) -> %d (%s) [%0.2f]\n", i,names[i], j, names[j], x[i,j];
  }
}
