/* 

  Curious set of integers problem in OPL.

  Martin Gardner (February 1967):
  """
  The integers 1,3,8, and 120 form a set with a remarkable property: the product of any 
  two integers is one less than a perfect square. Find a fifth number that can be added 
  to the set without destroying this property.
  """
  
  Solution: The number is 0.

  There are however other sets of five numbers with this property.
  Here are the one in the range of 0.10000:
  [0, 1, 3, 8, 120]
  [0, 1, 3, 120, 1680]
  [0, 1, 8, 15, 528]
  [0, 1, 8, 120, 4095]
  [0, 1, 15, 24, 1520]
  [0, 1, 24, 35, 3480]
  [0, 1, 35, 48, 6888]
  [0, 2, 4, 12, 420]
  [0, 2, 12, 24, 2380]
  [0, 2, 24, 40, 7812]
  [0, 3, 5, 16, 1008]
  [0, 3, 8, 21, 2080]
  [0, 3, 16, 33, 6440]
  [0, 4, 6, 20, 1980]
  [0, 4, 12, 30, 5852]
  [0, 5, 7, 24, 3432]
  [0, 6, 8, 28, 5460]
  [0, 7, 9, 32, 8160]



  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 5;
// decision variables
dvar int x[1..n] in 0..10000;
dvar int p[1..n,1..n] in 0..100000; // temporary variables


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

   allDifferent(x);

   // increasing(x)
   forall(i in 2..n) {
     x[i-1] < x[i];
   }

   forall(i, j in 1..n: i !=j) {
      p[i,j]*p[i,j]-1 == (x[i]*x[j]);
   }

   // Handle the "holes" in the constraint above
   forall(i in 1..n) {
      p[i,i] == 1;
   }

   // Symmetry breaking
   // (Comment this to generate other set of numbers with this property.)
   (x[1] < 1 && x[2] == 1 && x[3] == 3 && x[4] == 8 && x[5] == 120)
   || 
   (x[1] == 1 && x[2] == 3 && x[3] == 8 && x[4] == 120 && x[5] > 120) ;


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x: ", t.x);
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
