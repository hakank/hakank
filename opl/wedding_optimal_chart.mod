/* 

  Finding an optimal wedding seating chart in OPL.

  From 
  Meghan L. Bellows and J. D. Luc Peterson
  "Finding an optimal seating chart for a wedding"
  http://www.improbable.com/news/2012/Optimal-seating-chart.pdf
  http://www.improbable.com/2012/02/12/finding-an-optimal-seating-chart-for-a-wedding
  
  """
  Every year, millions of brides (not to mention their mothers, future 
  mothers-in-law, and occasionally grooms) struggle with one of the 
  most daunting tasks during the wedding-planning process: the 
  seating chart. The guest responses are in, banquet hall is booked, 
  menu choices have been made. You think the hard parts are over, 
  but you have yet to embark upon the biggest headache of them all. 
  In order to make this process easier, we present a mathematical 
  formulation that models the seating chart problem. This model can 
  be solved to find the optimal arrangement of guests at tables. 
  At the very least, it can provide a starting point and hopefully 
  minimize stress and arguments… 
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int m = 17; // number of quests

int n = 5; // max number of tables
int a = 4; // maximum number of guests a table can seat
int b = 2; // minimum number of people each guest knows at their table


// Easier problem
/*
int n = 2; // max number of tables
int a = 10; // maximum number of guests a table can seat
int b = 1; // minimum number of people each guest knows at their table
*/


//  j   Guest         Relation
//  -------------------------------------
//  1   Deb           mother of the bride
//  2   John          father of the bride
//  3   Martha        sister of the bride
//  4   Travis        boyfriend of Martha
//  5   Allan         grandfather of the bride
//  6   Lois          wife of Allan
//  7   Jayne         aunt of the bride
//  8   Brad          uncle of the bride
//  9   Abby          cousin of the bride
// 10   Mary Helen    mother of the groom
// 11   Lee           father of the groom
// 12   Annika        sister of the groom
// 13   Carl          brother of the groom
// 14   Colin         brother of the groom
// 15   Shirley       grandmother of the groom
// 16   DeAnn         aunt of the groom
// 17   Lori          aunt of the groom
//              Table 2: Guest List
//
// C[j,k]: Connection matrix, indicating relation of guest j to
//         guest k (0..50 where 0 is no relation, 50 is strong relation)
//
int C[1..m, 1..m] = 
[
[ 1,50, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[50, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[ 1, 1, 1,50, 1, 1, 1, 1,10, 0, 0, 0, 0, 0, 0, 0, 0],
[ 1, 1,50, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[ 1, 1, 1, 1, 1,50, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[ 1, 1, 1, 1,50, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[ 1, 1, 1, 1, 1, 1, 1,50, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[ 1, 1, 1, 1, 1, 1,50, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[ 1, 1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,50, 1, 1, 1, 1, 1, 1],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0,50, 1, 1, 1, 1, 1, 1, 1],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1]
];


// decision variables
dvar int tables[1..m] in 1..n; // the placements
dvar int z in 0..sum(j, k in 1..m) C[j,k]; // to maximize


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(tables,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.valueSuccessRate())
                       );
  cp.setSearchPhases(s);

}

maximize z;
constraints {

   z == sum(ordered j,k in 1..m) (C[j,k]*(tables[j]==tables[k]));
   
   forall(i in 1..n) {
     sum(ordered j, k in 1..m) (C[j,k] > 0 && tables[j] == i && 
                                tables[k] == i) >= b;

     sum(j in 1..m) (tables[j] == i) <= a;
   }
   
   // symmetry breaking
   // tables[3] == 1; // Martha sits at table 1
   tables[1] == 1;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("tables:", t.tables);
        writeln("z: ", t.z);
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
