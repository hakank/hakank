/* 

  Four Islands puzzle (Dell Logic Puzzle) in OPL.

  http://brownbuffalo.sourceforge.net/FourIslandsClues.html
  """
  Title: Four Islands
  Author: Humphrey Dudley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1
  
  A tiny nation in the South Pacific contains four islands connected by bridges
  as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
  boasts a different primary export (alabaster, bananas, coconuts, and durian
  fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
  stadium, and koala preserve). Can you find the name, export, and tourist 
  attraction of each island on the map?
  
    N
  W   E     *compass directions
    S
  
  A, B, C, D are the islands
  
  (A) -- (B)
   |      |
   |      |
  (C) -- (D)
  
  
  1. The island noted for its koala preserve is due south of Pwana.
  2. The island with the largest alabaster quarry is due west of Quero.
  3. The island with the resort hotel is due east of the one that exports 
     durian fruit.
  4. Skern and the island with the jai alai stadium are connected by a 
     north-south bridge. 
  5. Rayou and the island that exports bananas are connected by an east-west
     bridge.
  6. The islands noted for the South Pacific's largest ice skating rink and 
     for the jai alai stadium are not connected by a bridge.
  
  Determine: Island island -- Island name -- Export -- Tourist Attraction
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;
range r = 1..4;

int A = 1;
int B = 2;
int C = 3;
int D = 4;



// decision variables

dvar int Pwana in r;
dvar int Quero in r;
dvar int Rayou in r;
dvar int Skern in r;
dvar int island[r] = [Pwana, Quero, Rayou, Skern];

dvar int alabaster in r;
dvar int bananas in r;
dvar int coconuts in r;
dvar int durian_fruit in r;
dvar int export[r] = [alabaster, bananas, coconuts, durian_fruit];

dvar int resort_hotel in r;
dvar int ice_skating_rink in r;
dvar int jai_alai_stadium in r;
dvar int koala_preserve in r;
dvar int attraction[r] = [resort_hotel, ice_skating_rink, jai_alai_stadium, koala_preserve];


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(island,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}



constraints {

  allDifferent(island);
  allDifferent(export);
  allDifferent(attraction);

  // 1. The island noted for its koala preserve is due south of Pwana.
  (
    (Pwana == A && koala_preserve == C)
    ||
    (Pwana == B && koala_preserve == D)
  );

  // 2. The island with the largest alabaster quarry is due west of Quero.
  ( 
    (alabaster == A && Quero == B) 
    || 
    (alabaster == C && Quero == D) 
  );

  // 3. The island with the resort hotel is due east of the one that exports 
  //    durian fruit.
  ( 
    ( durian_fruit == A && resort_hotel ==  B )
    ||
    ( durian_fruit == C && resort_hotel ==  D)
  );

  // 4. Skern and the island with the jai alai stadium are connected by a 
  //    north-south bridge. 
   (
     (Skern == A && jai_alai_stadium == C) 
     ||
     (Skern == C && jai_alai_stadium == A) 
     ||
     (Skern == B && jai_alai_stadium == D) 
     ||
     (Skern == D && jai_alai_stadium == B) 
   ); 

  // 5. Rayou and the island that exports bananas are connected by an 
  //    east-west bridge.
  (
    (Rayou == A && bananas == B) 
    ||
    (Rayou == B && bananas == A) 
    ||
    (Rayou == C && bananas == D) 
    ||
    (Rayou == D && bananas == C) 
  );

  // 6. The islands noted for the South Pacific's largest ice skating rink 
  //    and for the jai alai stadium are not connected by a bridge.
  ( 
   (ice_skating_rink == A && jai_alai_stadium == D)
   ||
   (ice_skating_rink == D && jai_alai_stadium == A)

   ||
   (ice_skating_rink == B && jai_alai_stadium == C)
   ||
   (ice_skating_rink == C && jai_alai_stadium == B)
  );


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("island    : ", t.island);
        writeln("export    : ", t.export);
        writeln("attraction: ", t.attraction);
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
