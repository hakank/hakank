/* 

  Safe cracking puzzle in OPL.

  From the Oz Primer:
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The code of Professor Smart's safe is a sequence of 9 distinct 
  nonzero digits C1 .. C9 such that the following equations and
  inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

   and

   C1 <> 1, C2 <> 2, ..., C9 <> 9

  can you find the correct combination?
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 9;

// decision variables
dvar int C1 in 1..n; 
dvar int C2 in 1..n; 
dvar int C3 in 1..n; 
dvar int C4 in 1..n; 
dvar int C5 in 1..n; 
dvar int C6 in 1..n; 
dvar int C7 in 1..n; 
dvar int C8 in 1..n; 
dvar int C9 in 1..n; 

dvar int x[1..9] in 1..n;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
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

constraints {

   x[1] == C1;
   x[2] == C2;
   x[3] == C3;
   x[4] == C4;
   x[5] == C5;
   x[6] == C6;
   x[7] == C7;
   x[8] == C8;
   x[9] == C9;


   allDifferent(x);

   C4 - C6 == C7;
   C1 * C2 * C3 == C8 + C9;
   C2 + C3 + C6 < C8;
   C9 < C8;

   forall(i in 1..9) {
     x[i] != i;
   }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
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
