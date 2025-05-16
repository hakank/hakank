/* 

  Ski assignment problem in OPL.

  From
  Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008 – Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  """
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
  decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
  They'll let you ski all winter for free in exchange for helping their ski rental 
  shop with an algorithm to assign skis to skiers. Ideally, each skier should 
  obtain a pair of skis whose height matches his or her own height exactly. 
  Unfortunately, this is generally not possible. We define the disparity between 
  a skier and his or her skis to be the absolute value of the difference between 
  the height of the skier and the pair of skis. Our objective is to find an 
  assignment of skis to skiers that minimizes the sum of the disparities. 
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
  following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.
%
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int num_skis = 6;
int num_skiers = 5;

int ski_heights[1..num_skis] =  [1, 2, 5, 7, 13, 21];
int skier_heights[1..num_skiers] = [3, 4, 7, 11, 18];

// decision variables
dvar int x[1..num_skiers] in 1..num_skis; 
dvar int z;

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
                        // f.selectLargest(f.value())
                        // f.selectRandomValue()
                       );
  cp.setSearchPhases(s);

}

minimize z;
constraints {

  allDifferent(x);

  z == sum(i in 1..num_skiers, j in 1..num_skis) 
             (x[i] == j) * abs(ski_heights[j] - skier_heights[i]);

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x: ", thisOplModel.x);
      writeln("z: ", thisOplModel.z);

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
      sol++;
   }

   cp.endSearch();
   writeln("#solutions: ", sol);

} 
