/* 

  Bus scheduling in OPL.

  Problem from Taha "Introduction to Operations Research", page 58.

  This is a slightly more general model than Taha's.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int time_slots = 6;
int demands[1..time_slots] = [8, 10, 7, 12, 4, 4];


// decision variables
dvar int x[1..time_slots] in 0..max(i in 1..time_slots) demands[i];
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

  z == sum(i in 1..time_slots) x[i];

  // meet the demands for this and the next time slot
  forall(i in 1..time_slots-1) x[i]+x[i+1] >= demands[i];

  // demand "around the clock"
  x[time_slots] + x[1] >= demands[time_slots];


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
