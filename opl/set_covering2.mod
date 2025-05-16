/* 

  Set covering in OPL.

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 8;
int num_streets = 11;

{int} corners[1..num_streets] = 
[
  {1,2},
  {2,3},
  {4,5},
  {7,8},
  {6,7},
  {2,6},
  {1,6},
  {4,7},
  {2,4},
  {5,8},
  {3,5}
];


// decision variables
dvar int x[1..n] in 0..1;
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

  z == sum(i in 1..n) x[i];

  forall(i in 1..num_streets) {
       sum(j in corners[i]) x[j] >= 1;
  }

   
}


main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("z: ", t.z);
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
