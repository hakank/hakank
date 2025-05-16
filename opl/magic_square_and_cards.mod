/* 

  Magic Squares and Cards in OPL.

  Martin Gardner (July 1971)
  """
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 3;

// decision variables
dvar int x[1..n, 1..n] in 1..13;
dvar int s in 0..13*4;

execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}

constraints {

  // there are 4 cards of each value in a deck
  forall(i in 1..13) {
     sum(r,c in 1..n) (x[r,c]==i) <= 4;
  }

  // the standard magic square constraints (sans all_different)
  forall (c in 1..n) { sum (r in 1..n) x[r, c] == s;}
  forall (r in 1..n) {sum (c in 1..n) x[r, c] == s;}
  sum(i in 1..n) x[i,i] == s;
  sum(i in 1..n) x[i, n + 1 - i] == s;
}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:     ", t.x);
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
