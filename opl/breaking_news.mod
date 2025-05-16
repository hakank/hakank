/* 

  Breaking News puzzle (Dell Logic Puzzles) in OPL.

  Problem from  
  http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
  """
  Title: Breaking News
  Author: Faith Johnson
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1

  The Daily Galaxy sent its four best reporters (Corey, Jimmy, Lois, and 
  Perry) to different locations (Bayonne, New Hope, Port Charles, and 
  South Amboy) to cover four breaking news events (30-pound baby, blimp 
  launching, skyscraper dedication, and beached whale). Their editor is 
  trying to remember where each of the reporters is. Can you match the name 
  of each reporter with the place he or she was sent, and the event that 
  each covered?
  
  1. The 30-pound baby wasn't born in South Amboy or New Hope.
  2. Jimmy didn't go to Port Charles.
  3. The blimp launching and the skyscraper dedication were covered, in some 
     order, by Lois and the reporter who was sent to Port Charles.
  4. South Amboy was not the site of either the beached whale or the 
     skyscraper dedication.
  5. Bayonne is either the place that Corey went or the place where the 
     whale was beached, or both.
  
  Determine: Reporter -- Location -- Story
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;
range r = 1..4;

int Corey = 1;
int Jimmy = 2;
int Lois = 3;
int Perry = 4;
int reporters[r] = [Corey, Jimmy, Lois, Perry];


// decision variables
dvar int Bayonne in r;
dvar int New_Hope in r;
dvar int Port_Charles in r;
dvar int South_Amboy in r;
dvar int locations[r] = [Bayonne, New_Hope, Port_Charles, South_Amboy];

dvar int baby in r;
dvar int blimp in r;
dvar int skyscraper in r;
dvar int whale in r;
dvar int events[r] = [baby, blimp, skyscraper, whale];



execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(locations,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}



constraints {

   allDifferent(locations);
   allDifferent(events);

   // 1. The 30-pound baby wasn't born in South Amboy or New Hope.
   baby != South_Amboy;
   baby != New_Hope;
 
   //  2. Jimmy didn't go to Port Charles.
   Jimmy != Port_Charles;

   // 3. The blimp launching and the skyscraper dedication were covered, 
   //    in some order, by Lois and the reporter who was sent to 
   //    Port Charles.
   (blimp == Lois && skyscraper == Port_Charles)
   ||
   (skyscraper == Lois && blimp == Port_Charles);

   //  4. South Amboy was not the site of either the beached whale or the 
   //     skyscraper  dedication.
   South_Amboy != whale;
   South_Amboy != skyscraper;

   //  5. Bayonne is either the place that Corey went or the place where 
   // the whale was beached, or both.
   (Bayonne == Corey) + (Bayonne == whale)  >= 1;


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("reporters: ", t.reporters);
        writeln("locations: ", t.locations);
        writeln("events   : ", t.events);
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
