/* 

  Furniture moving (sceduling) in OPL.

  Problem from Marriott & Stuckey: 
  "Programming with constraints", page  112f

  Note: This use a decomposition of cumulative constraints, not
        any of the many built-in functions for scheduling in OPL.


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;
int Durations[1..n] = [30,10,15,15];
int Resources[1..n] = [3,1,3,2];
int upperLimit = 80;

// decision variables
dvar int StartTimes[1..n] in 0..upperLimit;
dvar int EndTimes[1..n] in 0..upperLimit*2;
dvar int numPersons in 0..10; 
dvar int end_time in 0..upperLimit*2;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(StartTimes,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}

minimize 100*numPersons + end_time; // weights objective
// minimize numPersons;
// minimize end_time;

constraints {

   // cumulative(StartTimes, Durations, Resources , numPersons);
   // Note: This is a decomposition of cumulative
   forall(t in 0..upperLimit*2) {
      numPersons >= sum(task in 1..n) (
                      Resources[task]*(t >= StartTimes[task] && t < EndTimes[task])
                    );
   }

   // setting EndTimes
   forall(i in 1..n) { 
       EndTimes[i] == StartTimes[i] + Durations[i];
   }

   end_time == max(i in 1..n) EndTimes[i];

   //
   // Some extra constraints to play with:
   // 

   // How many persons if everything should start at the same time
   // forall(i in 1..n) { StartTimes[i] == 0; }

   // Must be finished in 60 minutes
   // forall(i in 1..n) {EndTimes[i] <= 60;} 

   // limitation of the number of people
   // numPersons <= 4;

   // Tasks must start on even 10 minutes 
   // forall(i in 1..n) {StartTimes[i] % 10 == 0;};

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {

      writeln();
      writeln("StartTimes: ", thisOplModel.StartTimes);
      writeln("Durations : ", thisOplModel.Durations);
      writeln("EndTimes  : ", thisOplModel.EndTimes);
      writeln("Resources : ", thisOplModel.Resources);
      writeln("numPersons: ", thisOplModel.numPersons);
      writeln("end_time  : ", thisOplModel.end_time); 
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
