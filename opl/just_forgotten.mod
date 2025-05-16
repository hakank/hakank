/* 

  Just forgotten puzzle (Enigma 1517) in OPL.

  From http://www.f1compiler.com/samples/Enigma%201517.f1.html
  """
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.

  Joe was furious when he forgot one of his bank account numbers. 
  He remembered that it had all the digits 0 to 9 in some order, so he tried
  the following four sets without success:

      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7 
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5

  When Joe finally remembered his account number, he realised that in each set
  just four of the digits were in their correct position and that, if one knew
  that, it was possible to work out his account number.
  What was it?
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int rows = 4;
int cols = 10;
int a[1..rows, 1..cols] = 
[
   [9,4,6,2,1,5,7,8,3,0],
   [8,6,0,4,3,9,1,2,5,7],
   [1,6,4,0,2,9,7,8,5,3],
   [6,8,2,4,3,1,9,0,7,5]
];

// decision variables
dvar int x[1..10] in 0..9;


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
   allDifferent(x);

   forall(r in 1..rows) {
     sum(c in 1..cols) (x[c] == a[r,c]) == 4;
   }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     var alpha = " abcdefghijklmnopqrstuvwy";
     while (cp.next()) {
        var t = thisOplModel;
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
