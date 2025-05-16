/* 

  Finding celebrities drops in OPL.

  From Uwe Hoffmann
  "Finding celebrities at a party"
  http://www.codemanic.com/papers/celebs/celebs.pdf
  """
  Problem: Given a list of people at a party and for each person the list of
  people they know at the party, we want to find the celebrities at the party. 
  A celebrity is a person that everybody at the party knows but that 
  only knows other celebrities. At least one celebrity is present at the party.
  """
  (This paper also has an implementation in Scala.)
  
  Note: The origin of this problem is 
    Richard Bird and Sharon Curtis: 
    "Functional pearls: Finding celebrities: A lesson in functional programming"
    J. Funct. Program., 16(1):13–20, 2006.


  The problem from Hoffmann's paper is to find of who are the 
  celebrity/celebrities in this party graph:

    Adam  knows {Dan,Alice,Peter,Eva},
    Dan   knows {Adam,Alice,Peter},
    Eva   knows {Alice,Peter},
    Alice knows {Peter},
    Peter knows {Alice}
  
  Solution: the celebrities are Peter and Alice.


  Note: Here we assume that a person know him/herself, since it makes the
        encoding somewhat easier.

  Note2: 
  I blogged about this problem in 
  "Finding celebrities at a party"
  http://www.hakank.org/constraint_programming_blog/2010/01/finding_celebrities_at_a_party.html



  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = ...;
{int} graph[1..n] = ...;


//
// The party graph of the example above:
//
//  Adam  knows {Dan,Alice,Peter,Eva},  {2,3,4,5}
//  Dan   knows {Adam,Alice,Peter},     {1,4,5}
//  Eva   knows {Alice,Peter},          {4,5}
//  Alice knows {Peter},                {5}
//  Peter knows {Alice}                 {4}
//
/*
int n = 5;
{int} graph[1..n] = 
[
  {2,3,4,5},
  {1,4,5},
  {4,5},
  {4,5},
  {4,5}
];
*/


// decision variables
dvar int celebrities[1..n] in 0..1;
// number of celebrities
dvar int num_celebrities in 0..n;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Basic"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(celebrities,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);


}

constraints {

  num_celebrities == sum(i in 1..n) celebrities[i];

  // there is at least one celebrity
  num_celebrities >= 1;

  forall(i in 1..n) {
     // all persons know the celebrities,
     (celebrities[i] == 1) => sum(j in 1..n) (i in graph[j]) == n;
     // and the celebrities only know celebrities
     (celebrities[i] == 1) => card(graph[i]) == num_celebrities;
  } 

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var t = thisOplModel;
      writeln("celebrities: ", t.celebrities);
      writeln("num_celebrities: ", t.num_celebrities);
      write("the celebrities: ");
      for(i = 1; i <= t.n; i++) {
         if (t.celebrities[i] == 1) write(i, " ");
      }
      writeln();
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
