/* 

  Divisible by 9 through 1 problem  in OPL.

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  " Solving Combinatory Problems with LINQ"
  """
  Find a number consisting of 9 digits in which each of the digits 
  from 1 to 9 appears only once. This number must also satisfy these 
  divisibility requirements:
  
   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should 
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining 
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily 
      be divisible by 1).
  """
  
  Also, see
  "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/


  This is a slighly more general model by supporting
  different bases.
  However, using base > 10 giver integer overflow.
  
  For base <= 10 there are solution (i.e. the array x) 
  for the following
    2: [1]
    4: [1, 2, 3] 
       [3, 2, 1]
    6: [1, 4, 3, 2, 5] 
       [5, 4, 3, 2, 1]
    8: [3, 2, 5, 4, 1, 6, 7]
       [5, 2, 3, 4, 7, 6, 1]
       [5, 6, 7, 4, 3, 2, 1]
   10: [3, 8, 1, 6, 5, 4, 7, 2, 9]a
   12: <no solution>
   14: [9, 12, 3, 10, 5, 4, 7, 6, 11, 8, 1, 2, 13]


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int base = 10;
int n = base - 1;
int m = ftoi(pow(base, n))-1;

// decision variables
dvar int x[1..n] in 1..n; // the digits
dvar int t[1..n] in 0..m; // the numbers. t[1] contains the answer

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
   forall(i in 1..n) {
       t[i] == sum(j in 1..base-i) ftoi(base^(base-i-j))*x[j];
       t[base-i] % i == 0;
   }

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x: ", thisOplModel.x);
      writeln("t: ", thisOplModel.t);
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
