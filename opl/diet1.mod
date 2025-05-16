/* 

  Diet problem in OPL.

  Minimize the cost for the products:
  Type of                        Calories   Chocolate    Sugar    Fat
  Food                                      (ounces)     (ounces) (ounces)
  Chocolate Cake (1 slice)       400           3            2      2
  Chocolate ice cream (1 scoop)  200           2            2      4
  Cola (1 bottle)                150           0            4      1
  Pineapple cheesecake (1 piece) 500           0            4      5

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;
int price[1..n] = [ 50, 20, 30, 80]; // in cents
int limits[1..n] = [500,  6, 10,  8]; // requirements for each nutrition type

// nutritions for each product
int calories[1..n]  = [400, 200, 150, 500];
int chocolate[1..n] = [3,2,0,0];
int sugar[1..n]     = [2,2,4,4];
int fat[1..n]       = [2,4,1,5];

// decision variables
dvar int x[1..n]; // number of items of each dish
dvar int cost;

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

minimize cost;
constraints {

   cost == sum(i in 1..n) x[i]*price[i];
   
   forall(i in 1..n) x[i] >= 0;
   sum(i in 1..n) x[i]*calories[i]  >= limits[1];
   sum(i in 1..n) x[i]*chocolate[i] >= limits[2];
   sum(i in 1..n) x[i]*sugar[i]     >= limits[3];
   sum(i in 1..n) x[i]*fat[i]       >= limits[4];

   // cost <= 90 // for all solutions

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x: ", thisOplModel.x);
      writeln("cost: ", thisOplModel.cost);
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
