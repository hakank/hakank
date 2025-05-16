/* 

  Set Covering Deployment in OPL.

  See
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

  which guarantees that if v_i does not contain a field army, it has a neighbor with 
  two field armies, and
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

  In Rubalcaba, R. R. "Fractional Domination, Fractional Packings, and 
                       Fractional Isomorphisms of Graphs." 
                       Ph.D. dissertation. Auburn, Alabama: Auburn University. 
                       May 13, 2005. 
                       http://webpages.uah.edu/~rubalcr/RUBALCABA.pdf.


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 8; // number of countries
// connections between the countries
int matrix[1..n, 1..n] = 
[
  [0, 1, 0, 1, 0, 0, 1, 1],
  [1, 0, 0, 1, 0, 0, 0, 0],
  [0, 0, 0, 0, 1, 1, 0, 0],
  [1, 1, 0, 0, 0, 0, 1, 0],
  [0, 0, 1, 0, 0, 1, 1, 0],
  [0, 0, 1, 0, 1, 0, 1, 1],
  [1, 0, 0, 1, 1, 1, 0, 1],
  [1, 0, 0, 0, 0, 1, 1, 0]
];


// decision variables
dvar int x[1..n] in 0..1; // the first army
dvar int y[1..n] in 0..1; // the second army
dvar int c_armies; // number of armies needed (to minimize)



execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

}


minimize c_armies;
constraints {

   c_armies == sum(i in 1..n) (x[i] + y[i]);

   // Constraint 1: There is always an army in a city (+ maybe a backup)
   //               Or rather: Is there a backup, there must be an an army
   forall(i in 1..n) {
      x[i] >= y[i];
   }

   // Constraint 2: 
   // There should always be an backup army near every city
   forall(i in 1..n) {
       (x[i] + sum(j in 1..n: matrix[i,j] == 1) y[j]) >= 1;
   }


  // Constraint 3: for showing all the solutions. 
  // sum(i in 1..n) (x[i] + y[i]) <= 4;

}


main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("y:", t.y);
        writeln("c_armies: ", t.c_armies);
        writeln();
        sol++;
     }
     cp.endSearch();

     writeln();
     var info = cp.info;
     writeln("#ModelVariables: ", info.NumberOfModelVariables);
     writeln("#Variables: ", info.NumberOfVariables);
     writeln("#Constraints: ", info.NumberOfConstraints);
     writeln("#Choice points: ", info.NumberOfChoicePoints);
     writeln("#Fails: ", info.NumberOfFails);
     writeln("#Branches: ", info.NumberOfBranches);
     writeln("TotalTime: ", info.TotalTime);
     writeln("SolveTime: ", info.SolveTime);
     writeln();

     writeln("#solutions: ", sol);

} 
