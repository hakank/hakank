/* 

  Magic sequence in OPL.

  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 0 and n-1, 
  such that for all i in 0 to n-1, the number i occurs exactly xi times in the sequence. 
  For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence since 0 occurs 6 times in it, 
  1 occurs twice, ...
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// int n = 1000; // Takes about 3.5s 997 failures
int n = 100;

// decision variables
dvar int x[0..n-1] in 0..n-1;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Medium"; // "Default", "Low", "Basic", "Medium", "Extended";
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

     forall(i in 0..n-1) {
        count(x, i) == x[i];
        // x[i] == sum(j in 0..n-1) (x[j]==i); // This is slow.
     }

     // redundant constraints
     sum(i in 0..n-1) x[i]   == n;
     sum(i in 0..n-1) x[i]*i == n;


}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x : ", thisOplModel.x);
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
