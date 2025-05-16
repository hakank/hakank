/* 

  "A Puzzle" in OPL.

  From "God plays dice"
  "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/

  And the sequel "Answer to a puzzle"
  http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/

  This problem instance was taken from the latter blog post.

  """
  8809 = 6
  7111 = 0
  2172 = 0
  6666 = 4
  1111 = 0
  3213 = 0
  7662 = 2
  9312 = 1
  0000 = 4
  2222 = 0
  3333 = 0
  5555 = 0
  8193 = 3
  8096 = 5
  7777 = 0
  9999 = 4
  7756 = 1
  6855 = 3
  9881 = 5
  5531 = 0

  2581 = ?
  """

  Note: 
  This model yields 10 solutions, since x4 is not 
  restricted in the constraints. 
  All solutions has x assigned to the correct result. 
  

  The problem stated in "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
  is
  """
  8809 = 6
  7662 = 2
  9312 = 1
  8193 = 3
  8096 = 5
  7756 = 1
  6855 = 3
  9881 = 5

  2581 = ?
  """
  This problem instance - using the same principle - yields 
  two different solutions of x, one is the same (correct) as 
  for the above problem instance, and one is not.
  This is because here both x4 and x1 are underdefined.
  
  Note: 
  This problem has another non-algebraic and - let's say - topological
  approach which yield the same solution as the first problem and one
  of the two solutions of the second problem.


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

range R = 0..9;

// decision variables
dvar int x0 in R;
dvar int x1 in R;
dvar int x2 in R;
dvar int x3 in R;
dvar int x4 in R;
dvar int x5 in R;
dvar int x6 in R;
dvar int x7 in R;
dvar int x8 in R;
dvar int x9 in R;

dvar int allx[1..10] = [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9];
dvar int x in R; // the unknown


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

  // This yields 10 solutions, all with the same
  // values for x
  x8+x8+x0+x9 == 6;
  x7+x1+x1+x1 == 0;
  x2+x1+x7+x2 == 0;
  x6+x6+x6+x6 == 4;
  x1+x1+x1+x1 == 0;
  x3+x2+x1+x3 == 0;
  x7+x6+x6+x2 == 2;
  x9+x3+x1+x2 == 1;
  x0+x0+x0+x0 == 4;
  x2+x2+x2+x2 == 0;
  x3+x3+x3+x3 == 0;
  x5+x5+x5+x5 == 0;
  x8+x1+x9+x3 == 3;
  x8+x0+x9+x6 == 5;
  x7+x7+x7+x7 == 0;
  x9+x9+x9+x9 == 4;
  x7+x7+x5+x6 == 1;
  x6+x8+x5+x5 == 3;
  x9+x8+x8+x1 == 5;
  x5+x5+x3+x1 == 0;

  x2+x5+x8+x1 == x;


  /*
  // This yields two different values for x
  x8+x8+x0+x9 == 6;
  x7+x6+x6+x2 == 2;
  x9+x3+x1+x2 == 1;
  x8+x1+x9+x3 == 3;
  x8+x0+x9+x6 == 5;
  x7+x7+x5+x6 == 1;
  x6+x8+x5+x5 == 3;
  x9+x8+x8+x1 == 5;

  x2+x5+x8+x1 == x;
  */

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("allx :", t.allx);
        writeln("x :", t.x);
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
