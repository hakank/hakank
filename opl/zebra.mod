/* 

  Zebra puzzle in OPL.

  From a MiniZinc model by Peter Stuckey
  """
  This is a puzzle which has been circulating the net. There are a couple
  different versions out there which try to be a little more politically
  correct but are essentially the same problem.    
  	1. There are five houses, each of a different color and inhabited by
	   men of different nationalities, with different pets, drinks,
	   and cigarettes.
  	2. The Englishman lives in the red house.
  	3. The Spaniard owns the dog.
  	4. Coffee is drunk in the green house.
  	5. The Ukrainian drinks tea.
  	6. The green house is immediately to the right of the ivory house.
  	7. The Old Gold smoker owns snails.
  	8. Kools are smoked in the yellow house.
  	9. Milk is drunk in the middle house.
  	10. The Norwegian lives in the first house on the left.
  	11. The man who smokes Chesterfields lives in the house next to the
	    man with the fox.
  	12. Kools are smoked in the house next to the house where the horse is
	    kept.
  	13. The Lucky Strike smoker drinks orange juice.
  	14. The Japanese smoke Parliaments.
  	15. The Norwegian lives next to the blue house.
  NOW, who drinks water? And who owns the zebra?
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 5; 
range Nationalities = 1..n;
int English = 1;
int Spanish = 2;
int Ukrainian = 3;
int Norwegian = 4;
int Japanese = 5;

string nationsString[1..n] = ["English", "Spanish", "Ukrainian", "Norwegian", "Japanese"];

range Colors = 1..n;
int Red = 1;
int Green = 2;
int Ivory = 3;
int Yellow = 4;
int Blue = 5;

range Animals = 1..n;
int Dog = 1;
int Fox = 2;
int Horse = 3;
int Zebra = 4;
int Snails = 5;

range Drinks = 1..n;
int Coffee = 1;
int Tea = 2;
int Milk = 3;
int OrangeJuice = 4;
int Water = 5;


range Cigarettes = 1..n;
int OldGold = 1;
int Kools = 2;
int Chesterfields = 3;
int LuckyStrike = 4;
int Parliaments = 5;

range Houses = 1..5;

// decision variables
dvar int nation[Nationalities] in Houses;
dvar int color[Colors] in Houses;
dvar int animal[Animals] in Houses;
dvar int drink[Drinks] in Houses;
dvar int smoke[Cigarettes] in Houses;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(color,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);


}


constraints {
	allDifferent(nation);
	allDifferent(color);
	allDifferent(animal);
	allDifferent(drink);
	allDifferent(smoke);

	nation[English] == color[Red];
	nation[Spanish] == animal[Dog];
	drink[Coffee] == color[Green];
        nation[Ukrainian] == drink[Tea];
        color[Green] == color[Ivory] + 1;
        smoke[OldGold] == animal[Snails];
        smoke[Kools] == color[Yellow];
        drink[Milk] == 3; // middle
        nation[Norwegian] == 1; // left
        abs(smoke[Chesterfields] - animal[Fox]) == 1; // next to
        abs(smoke[Kools] - animal[Horse]) == 1; // next to
        smoke[LuckyStrike] == drink[OrangeJuice];
        nation[Japanese] == smoke[Parliaments];
        abs(nation[Norwegian] - color[Blue]) == 1; // next to


}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;

   while (cp.next()) {
      writeln("nation: ", thisOplModel.nation);
      writeln("color : ", thisOplModel.color);
      writeln("animal: ", thisOplModel.animal);
      writeln("drink : ", thisOplModel.drink);
      writeln("smoke : ", thisOplModel.smoke);

      // NOW, who drinks water? And who owns the zebra?
      var n = thisOplModel.n;
      var nation = thisOplModel.nation;
      var drink = thisOplModel.drink;
      var animal = thisOplModel.animal;
      var Water = thisOplModel.Water;
      var Zebra = thisOplModel.Zebra;
      var nationsString = thisOplModel.nationsString;
      for(i = 1; i <= n; i++) {
         if (nation[i] == drink[Water]) {
            writeln("The ", nationsString[i], " drinks Water (house ", nation[i], ").");
         }
      }

      for(i = 1; i <= n; i++) {
         if (nation[i] == animal[Zebra]) {
            writeln("The ", nationsString[i], " owns Zebra (house ", nation[i], ").");
         }
      }

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
