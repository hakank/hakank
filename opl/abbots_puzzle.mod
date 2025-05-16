/* 

  The Abbot's Puzzle in OPL.

  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  The Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.
  
  If 100 bushels of corn were distributed among 100 people in such a
  manner that each man received three bushels, each woman two, and each
  child half a bushel, how many men, women, and children were there?
  
  Dudeney added the condition that there are five times as many women as
  men. That way, the solution becomes unique (otherwise, there are seven
  solutions). 
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// decision variables
dvar int M;
dvar int W;
dvar int C;
dvar int the_sum;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  /*
  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);
  */
 
}

constraints {

   the_sum == M+W+C;

   the_sum == 100;
   M >= 0;
   W >= 0;
   C >= 0;
   // M * 6 + W * 4 + C == 200; // multiply with 2
   M * 3 + W * 2 + C/2 == 100; // OPL handles the division without a problem
   M * 5 == W;


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("the_sum:", t.the_sum);
        writeln("M:", t.M);
        writeln("W:", t.W);
        writeln("C:", t.C);

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
