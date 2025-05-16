/* 

  Secret Santa problem  in OPL.

  From Ruby Quiz Secret Santa
  http://www.rubyquiz.com/quiz2.html
  """
  Honoring a long standing tradition started by my wife's dad, my friends 
  all play a Secret Santa game around Christmas time. We draw names and 
  spend a week sneaking that person gifts and clues to our identity. On the 
  last night of the game, we get together, have dinner, share stories, and, 
  most importantly, try to guess who our Secret Santa was. It's a crazily 
  fun way to enjoy each other's company during the holidays.
  
  To choose Santas, we use to draw names out of a hat. This system was 
  tedious, prone to many "Wait, I got myself..." problems. This year, we 
  made a change to the rules that further complicated picking and we knew 
  the hat draw would not stand up to the challenge. Naturally, to solve 
  this problem, I scripted the process. Since that turned out to be more 
  interesting than I had expected, I decided to share.
  
  This weeks Ruby Quiz is to implement a Secret Santa selection script.
  
  Your script will be fed a list of names on STDIN. 
  ...
  Your script should then choose a Secret Santa for every name in the list. 
  Obviously, a person cannot be their own Secret Santa. In addition, my friends 
  no longer allow people in the same family to be Santas for each other and your 
  script should take this into account.
  """

  Comment: Well, this model skips the file input and mail parts. We 
           assume that the friends are identified with a number from 1..n,
           and the families is identified with a number 1..num_families. 


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// The Ruby Quiz example
/*
int n = 7;
int num_families = 4;
int family[1..n] = [1,1,2,2, 3, 4,4];
*/

// Slightly harder example
int n = 12;
int num_families = 4;
int family[1..n] = [1,1,1,1, 2, 3,3,3,3,3, 4,4];


// decision variables
dvar int x[1..n] in 1..n;



execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
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

  // Everyone gives and receives a Secret Santa
  allDifferent(x);

  // Can't be one own's Secret Santa
  // no_fix_points(x) 
  forall(i in 1..n) x[i] != i;

  // No Secret Santa to a person in the same family
  forall(i in 1..n) {
     family[i] != family[x[i]];
  }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

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
