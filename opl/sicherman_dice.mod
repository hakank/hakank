/* 

  Sicherman dice in OPL.

  From http://en.wikipedia.org/wiki/Sicherman_dice
  """ 
  Sicherman dice are the only pair of 6-sided dice which are not normal dice, 
  bear only positive integers, and have the same probability distribution for 
  the sum as normal dice.
  
  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  """

  Note: I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/

  This model gets the two different ways, first the standard way and
  then the Sicherman dice:
  
  x1 = [1, 2, 3, 4, 5, 6]
  x2 = [1, 2, 3, 4, 5, 6]
  ----------
  x1 = [1, 2, 2, 3, 3, 4]
  x2 = [1, 3, 4, 5, 6, 8]


  Extra: If we also allow 0 (zero) as a valid value then the 
  following two solutions are also valid:
  
  x1 = [0, 1, 1, 2, 2, 3]
  x2 = [2, 4, 5, 6, 7, 9]
  ----------
  x1 = [0, 1, 2, 3, 4, 5]
  x2 = [2, 3, 4, 5, 6, 7]
  
  These two extra cases are mentioned here:
  http://mathworld.wolfram.com/SichermanDice.html


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6; 
int m = 10; // max integer 
int min_val = 0; // minimum value (see comment above)

// standard distribution
int standard_dist[2..12] = [1,2,3,4,5,6,5,4,3,2,1];


// decision variables
// the two dice
dvar int x1[1..n] in min_val..m;
dvar int x2[1..n] in min_val..m;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x1,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
  var s2 = f.searchPhase(x2,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s,s2);

}


constraints {

   // ensure that the distribution of the dice are as standard dist.
   forall(k in 2..12) {
      standard_dist[k] == sum(i,j in 1..n) ( x1[i]+x2[j] == k);
   }

   // symmetry breaking
   // increasing x1 and x2
   forall(i in 2..n) {
     x1[i-1] <= x1[i];
     x2[i-1] <= x2[i];
   }

   // x1 is less or equal to x2
   forall(i in 1..n) {
      x1[i] <= x2[i];
   }
   
   lex(x1, x2); 

}


main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     while (cp.next()) {
        var t = thisOplModel;
        writeln("x1:", t.x1);
        writeln("x2:", t.x2);
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
