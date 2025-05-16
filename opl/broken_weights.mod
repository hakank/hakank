/* 

  Broken weights problem in OPL.

  From
  http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first 
  posed by Claude Gaspard Bachet de Méziriac in a book of arithmetic problems 
  published in 1612, and can also be found in Heinrich Dorrie’s 100 
  Great Problems of Elementary Mathematics.
  
      A merchant had a forty pound measuring weight that broke 
      into four pieces as the result of a fall. When the pieces were 
      subsequently weighed, it was found that the weight of each piece 
      was a whole number of pounds and that the four pieces could be 
      used to weigh every integral weight between 1 and 40 pounds. What 
      were the weights of the pieces?
  
  Note that since this was a 17th-century merchant, he of course used a 
  balance scale to weigh things. So, for example, he could use a 1-pound 
  weight and a 4-pound weight to weigh a 3-pound object, by placing the 
  3-pound object and 1-pound weight on one side of the scale, and 
  the 4-pound weight on the other side.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;  // number of different weights
int m = 40; // original weight

// decision variables
dvar int weights[1..n] in 1..m;  // the weights
dvar int x[1..m, 1..n] in -1..1; // the combinations

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(weights,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


constraints {

    sum(i in 1..n) weights[i] == m;

    // Check that all weights from 1 to 40 can be made.
    //  
    // Since all weights can be on either side
    // of the side of the scale we allow either
    // -1, 0, or 1 or the weights, assuming that
    // -1 is the weights on the left and 1 is on the right.
    // 
    forall(j in 1..m) {
      sum(i in 1..n) x[j,i]*weights[i] == j;
    }

    // symmetry breaking
    forall(i in 2..n) {
      weights[i-1] <= weights[i];
    }


}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var x = thisOplModel.x;
      writeln("weights: ", thisOplModel.weights);
      // writeln("x: ", x);
      for(j = 1; j <= thisOplModel.m; j++) {
        if (j < 10) {
          write(" ");
        }
        write(j, ": ");
        for(i = 1; i <= thisOplModel.n; i++) {
          var v = x[j][i];
          if (v >= 0) {
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
