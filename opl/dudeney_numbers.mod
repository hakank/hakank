/* 

  Dudeney numbers in OPL.

  From Pierre Schaus blog post
  "Dudeney number"
  http://cp-is-fun.blogspot.com/2010/09/test-python.html
  """
  I discovered yesterday Dudeney Numbers
  A Dudeney Numbers is a positive integer that is a perfect cube such that the sum 
  of its decimal digits is equal to the cube root of the number. There are only six 
  Dudeney Numbers and those are very easy to find with CP.
  I made my first experience with google cp solver so find these numbers (model below) 
  and must say that I found it very convenient to build CP models in python!
  When you take a close look at the line: 
      solver.Add(sum([10**(n-i-1)*x[i] for i in range(n)]) == nb)
  It is difficult to argue that it is very far from dedicated 
  optimization languages!
  """
  
  Also see: http://en.wikipedia.org/wiki/Dudeney_number


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6;

// decision variables
dvar int x[1..n] in 0..9;
dvar int nb in 1..ftoi(10^n);
dvar int s in 1..9*n+1;

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

   nb == s*s*s;
   nb == sum(i in 1..n) ftoi(pow(10,(n-i)))*x[i];
   s == sum(i in 1..n) x[i];
}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("nb:", t.nb);
        writeln("s:", t.s);
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
