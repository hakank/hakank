/* 

  Magic squares in OPL.

  See 
  - http://en.wikipedia.org/wiki/Magic_square
  - http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob019/index.html


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 11; 
int total = ftoi((n*(n*n + 1)) / 2);

// decision variables
dvar int x[1..n, 1..n] in 1..n*n;



execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose

  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        // f.selectSmallest(f.value())
                        f.selectRandomValue()
                       );
  cp.setSearchPhases(s);


}


constraints {

  allDifferent(x);

  // rows/columns
  forall(j in 1..n) {
     sum(i in 1..n) x[i,j] == total;
     sum(i in 1..n) x[j,i] == total;
  }
  // diagonals
  sum(i in 1..n) x[i,i] == total; 
  sum(i in 1..n) x[i,n-i+1] == total; 



}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x:     ", thisOplModel.x);
      writeln("total: ", thisOplModel.total);
      writeln();
      sol++;
      if (thisOplModel.n > 4) {
        break;
      }
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
