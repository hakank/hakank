/* 

  Secret Santa problem II in OPL.

  From Maple Primes: "Secret Santa Graph Theory"
  http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
  """
  Every year my extended family does a "secret santa" gift exchange. 
  Each person draws another person at random and then gets a gift for 
  them. At first, none of my siblings were married, and so the draw was 
  completely random. Then, as people got married, we added the restriction 
  that spouses should not draw each others names. This restriction meant 
  that we moved from using slips of paper on a hat to using a simple 
  computer program to choose names. Then people began to complain when 
  they would get the same person two years in a row, so the program was 
  modified to keep some history and avoid giving anyone a name in their 
  recent history. This year, not everyone was participating, and so after 
  removing names, and limiting the number of exclusions to four per person, 
  I had data something like this:
  
  Name: Spouse, Recent Picks
  
  Noah: Ava. Ella, Evan, Ryan, John
  Ava: Noah, Evan, Mia, John, Ryan
  Ryan: Mia, Ella, Ava, Lily, Evan
  Mia: Ryan, Ava, Ella, Lily, Evan
  Ella: John, Lily, Evan, Mia, Ava
  John: Ella, Noah, Lily, Ryan, Ava
  Lily: Evan, John, Mia, Ava, Ella
  Evan: Lily, Mia, John, Ryan, Noah
  """
  
  Note: I interpret this as the following three constraints:
    1) One cannot be a Secret Santa of one's spouse
    2) One cannot be a Secret Santa for somebody two years in a row
    3) Optimization: maximize the time since the last time 

  This model also handle single persons, something the original
  problem don't mention.
  


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// int n = 8; // Without Single person
int n = 9; // With a Single person
int Noah = 1;
int Ava  = 2;
int Ryan = 3;
int Mia  = 4;
int Ella = 5;
int John = 6;
int Lily = 7;
int Evan = 8;
int Single = 9;

int spouses[1..n] = 
[
    Ava,  // Noa
    Noah, // Ava
    Mia,  // Rya
    Ryan, // Mia
    John, // Ella
    Ella, // John
    Evan, // Lily
    Lily  // Evan
    , 0     // Single has no spouse
]; 

int M = n+1; // "large M" to indicate no earlier history

//
// The matrix version of earlier rounds.
// M means that no earlier Santa.
// Note: Ryan and Mia has the same recipient for years 3 and 4,
//       and Ella and John has for year 4. 
//       This seems to be caused by modification of 
//       original data.
//
//
// rounds with a single person (fake data)
//
int rounds[1..n, 1..n] = 
[
//N  A  R  M  El J  L  Ev S
 [0, M, 3, M, 1, 4, M, 2, 2], // Noah
 [M, 0, 4, 2, M, 3, M, 1, 1], // Ava 
 [M, 2, 0, M, 1, M, 3, 4, 4], // Ryan
 [M, 1, M, 0, 2, M, 3, 4, 3], // Mia 
 [M, 4, M, 3, 0, M, 1, 2, M], // Ella
 [1, 4, 3, M, M, 0, 2, M, M], // John
 [M, 3, M, 2, 4, 1, 0, M, M], // Lily
 [4, M, 3, 1, M, 2, M, 0, M], // Evan
 [1, 2, 3, 4, M, 2, M, M, 0]  // Single
];


// decision variables
dvar int santas[1..n] in 1..n;
dvar int santas2[1..n] in 1..n;
dvar int santa_distance[1..n] in 1..n+1;
dvar int z; // total distance (to minimize)


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(santas,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


maximize z;
constraints {

  // Everyone gives and receives a Secret Santa
  allDifferent(santas);

   // no Santa for a spouses
   forall(i in 1..n) {
      santas[i] != i;
      if (spouses[i] > 0) {
         santas[i] != spouses[i];
      }
   }

   // optimize "distance" to earlier rounds:
   forall(i in 1..n) {
     santa_distance[i] == rounds[i,santas[i]];
   }

   // cannot be a Secret Santa for the same person two years in a row.
   forall(i in 1..n) {
       rounds[i,santas2[i]] == 1 && santas[i] != santas2[i];
   }

   z == sum(i in 1..n) santa_distance[i];


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("santas        :", t.santas);
        // writeln("santas2:", t.santas2);
        writeln("santa_distance:", t.santa_distance);
        writeln("z             : ", t.z);
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
