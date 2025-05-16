/* 

  Coin application in OPL.

  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  In this model we also decide which denominations that should be used.


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// the original problem (page 99)
int n = 6; // number of different coins
// int variables[1..n] = [1, 2, 5, 10, 25, 50]; 

// decision variables
dvar int x[1..n] in 0..99; // array for the changes
dvar int num_coins;  // number of coins used

// Which distribution of the variables is the best?
dvar int variables[1..n] in 1..99;

dvar int tmp[1..99, 1..n] in 0..99; // Temporary matrix for the changes of each value


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Medium"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s1 = f.searchPhase(variables,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
 
  var s2 = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s1,s2);



}

minimize num_coins;
constraints {

        num_coins == sum(i in 1..n) x[i];

        // This is the "main loop"        
        // Checks that all changes from 1 to 99 can be made.
        forall(j in 1..99) {
          sum(i in 1..n) tmp[j, i]*variables[i] == j;
          forall(i in 1..n) {
              tmp[j, i] <= x[i];
          }
        }

        // The variables.
        allDifferent(variables);
        sum(i in 1..n) variables[i] <= 99;
        forall(i in 2..n) {
          variables[i-1] < variables[i];
        }

        // Testing
        // variables[1] == 1;  variables[2] == 2;  variables[3] == 5;
        // variables[4] == 10; variables[5] == 25; variables[6] == 50;

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("variables : ", thisOplModel.variables);
      writeln("x : ", thisOplModel.x);
      // writeln("tmp: ", thisOplModel.tmp);
      writeln("num_coins: ", thisOplModel.num_coins);
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
