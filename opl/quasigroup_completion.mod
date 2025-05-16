/* 

  Quasigroup completion problem in OPL.

  See 
  Carla P. Gomes and David Shmoys:
  "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"

  
  See also
  Ivars Peterson "Completing Latin Squares"
  http://www.maa.org/mathland/mathtrek_5_8_00.html
  """
  Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
  a four-by-four array so that no column or row contains the same two numbers. 
  The result is known as a Latin square.
  ...
  The so-called quasigroup completion problem concerns a table that is correctly 
  but only partially filled in. The question is whether the remaining blanks in 
  the table can be filled in to obtain a complete Latin square (or a proper 
  quasigroup multiplication table).
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// Example from Ruben Martins and Inès Lynce
// Breaking Local Symmetries in Quasigroup Completion Problems, page 3
// The solution is unique:
//   1 3 2 5 4
//   2 5 4 1 3
//   4 1 3 2 5
//   5 4 1 3 2
//   3 2 5 4 1

int n = 5;
int m[1..n, 1..n] = 
 [
  [1, 0, 0, 0, 4],
  [0, 5, 0, 0, 0],
  [4, 0, 0, 2, 0],
  [0, 4, 0, 0, 0],
  [0, 0, 5, 0, 1]
 ];


// decision variables
dvar int x[1..n, 1..n] in 1..n;


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


constraints {

  forall(i,j in 1..n: m[i,j] > 0) {
     x[i,j] == m[i,j];
  }

  // make it a Latin Square
  forall(i in 1..n) {
      allDifferent(all(j in 1..n) x[i,j]);
      allDifferent(all(j in 1..n) x[j,i]);
  }

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {

      var x = thisOplModel.x;
      var n = thisOplModel.n;
      writeln();
      for(var i = 1; i <= n; i++) {
        write("  ");
        for(var j = 1; j <= n; j++) {
           var v = x[i][j];
           if (v < 10) {
             write(" ");
           }
           write(v, " ");
        }
        writeln();
      }
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
