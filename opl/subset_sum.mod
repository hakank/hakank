/* 

  Subset sum problem in OPL.

  From Katta G. Murty: "Optimization Models for Decision Making", page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  """
  Example 7.8.1
  
  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6;
int coins[1..n] = [16, 17, 23, 24, 39, 40];
int total = 100;



// decision variables
dvar int x[1..n] in 0..sum(i in 1..n) coins[i];


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

   total == sum(i in 1..n) coins[i]*x[i];

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("\nx:", t.x);
        writeln("total:", t.total);
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
