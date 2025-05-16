/* 

  Simple set covering in OPL.

  Placing of firestations, 
  from Winston "Operations Research", page 486

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int min_distance = 15;
int num_cities = 6;

int distance[1..num_cities, 1..num_cities] = 
[
 [ 0,10,20,30,30,20],
 [10, 0,25,35,20,10],
 [20,25, 0,15,30,20],
 [30,35,15, 0,15,25],
 [30,20,30,15, 0,14],
 [20,10,20,25,14, 0]
];


// decision variables
dvar int x[1..num_cities] in 0..1; // where to place the firestations
dvar int z;


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

minimize z;
constraints {

  z == sum(i in 1..num_cities) x[i]; 
  forall(j in 1..num_cities) {
       sum(i in 1..num_cities: distance[i,j] <= min_distance) x[i] >= 1;
  }

   
}


main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("z:", t.z);
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
