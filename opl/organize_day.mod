/* 

  Organizing a day (scheduling) in OPL.

  Problem formulation:
  Slides on (finite domain) Constraint Logic Programming, page 38f
  http://www.icparc.ic.ac.uk/eclipse/reports/eclipse.ppt
  (via http://www.icparc.ic.ac.uk/eclipse/reports/ )


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;

// Work, Mail, Shop, Bank
int Durations[1..n] = [4,1,2,1];

// tasks that must be completed before some other task
int BeforeTasks[1..2, 1..2] = 
[
 [4,3],
 [2,1]
];


// decision variables
dvar int Begins[1..n] in 9..17; 
dvar int Ends[1..n] in 9..17 ;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(Begins,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}

constraints {

   forall(i in 1..n) { 
     Ends[i] == Begins[i] + Durations[i];
   }

   // no overlaps of the tasks
   forall(ordered t1, t2 in 1..n) {
        Begins[t1] + Durations[t1] <= Begins[t2] 
        ||
        Begins[t2] + Durations[t2] <= Begins[t1];
   }

   forall(t in 1..2) {
      Ends[BeforeTasks[t,1]] <= Begins[BeforeTasks[t,2]];
   }
   
   Begins[1] >= 11;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("Begins:", t.Begins);
        writeln("Ends:", t.Ends);
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
