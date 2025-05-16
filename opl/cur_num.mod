/* 

  Puzzle in OPL.

  From Martin Henz' collection of puzzles
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/#curious
  """
  Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.

  The number 48 has this peculiarity, that if you add 1 to it the result
  is a square number, and if you add 1 to its half, you also get a
  square number. Now, there is no limit to the numbers that have this
  peculiarity, and it is an interesting puzzle to find three more of
  them---the smallest possible numbers. What are they?
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int up = 4000000; // upper range limit
range r = 1..up;

// decision variables
dvar int X in r;
dvar int A in r;
dvar int B in r;
dvar int C in r;
dvar int D in r;
dvar int E in r;

// For the labeling and output
dvar int x[1..6] = [X,A,B,C,D,E];

execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
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

   X + 1 == A; // if you add 1 to it
   A == B * B; // the result is a square number
   X == 2 * C; // if you to its half
   C + 1 == D; // add 1
   D == E * E; // you also get a square number

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("X:", t.X);
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
