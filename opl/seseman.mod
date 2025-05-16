/* 

  Seseman's Convent Problem in OPL.

  This model (the MiniZinc encoding) is commented in the (Swedish) blog post
  Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
  http://www.hakank.org/webblogg/archives/001209.html

  For a (Swedish) discussion of this problem se
  Sesemans matematiska klosterproblem samt lite Constraint Logic Programming
  http://www.hakank.org/webblogg/archives/001084.html
  and
  Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
  (using Eclipse code)

    n is the length of a border
  There are (n-2)^2 "holes", i.e.
  there are n^2 - (n-2)^2 variables to find out.

  The simplest problem, n = 3 (n x n matrix)
  which is represented by the following matrix:

   a b c 
   d   e 
   f g h 
  
  Where the following constraints must hold:

    a + b + c = border_sum
    a + d + f = border_sum
    c + e + h = border_sum
    f + g + h = border_sum
    a + b + c + d + e + f = total_sum

  For larger matrices just the borders is summed.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 3;
int border_sum = n*n;

// decision variables
dvar int total_sum in 1..n*n*n*n; // = 32;
dvar int x[1..n,1..n] in 0..n*n;


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

   // 0:s all the middle cells
   forall(i,j in 2..n-1 ) {
      x[i,j] == 0;
   }

   // sum the borders (border_sum)
   sum(i in 1..n) x[i,1] == border_sum;
   sum(i in 1..n) x[i,n] == border_sum;
   sum(j in 1..n) x[1,j] == border_sum;
   sum(j in 1..n) x[n,j] == border_sum;

   // all borders must be >= 1 (may be changed to 0 or whatever)
   forall(i,j in 1..n: i == 1 || j == 1 || i == n || j == n) {
      x[i,j] >= 1;
   }

   // sum the total
   total_sum == sum(i,j in 1..n) x[i,j];

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln();
      var n = thisOplModel.n;
      var x = thisOplModel.x;
      // writeln("x: ", x);
      for(var i = 1; i <= n; i++) {
        write("  ");
        for(var j = 1; j <= n; j++) {
           write(x[i][j], " ");
        }
        writeln();
      }
      writeln("total_sum: ", thisOplModel.total_sum);
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
