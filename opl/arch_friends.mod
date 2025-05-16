/* 

  Arch Friends puzzle (Dell Logic Puzzles) in OPL.

  Problem formulation from 
  http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
  """
  Title: Arch Friends
  Author: Mark T. Zegarelli
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Harriet, upon returning from the mall, is happily describing her four shoe 
  purchases to her friend Aurora. Aurora just loves the four different kinds 
  of shoes that Harriet bought (ecru espadrilles, fuchsia flats, purple pumps, 
  and suede sandals), but Harriet can't recall at which different store (Foot 
  Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) she got each pair. 
  Can you help these two figure out the order in which Harriet bought each 
  pair of shoes, and where she bought each?

  1. Harriet bought fuchsia flats at Heels in a Handcart.
  2. The store she visited just after buying her purple pumps was not Tootsies.
  3. The Foot Farm was Harriet's second stop.
  4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
  
  Determine: Order - Shoes - Store 
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;

// decision variables

// shoes
dvar int ecru_espadrilles    in 1..n;
dvar int fuchsia_flats       in 1..n;
dvar int purple_pumps        in 1..n;
dvar int suede_sandals       in 1..n;
dvar int shoes[1..n] = [ecru_espadrilles, fuchsia_flats, purple_pumps, suede_sandals];

// shops
dvar int Foot_Farm           in 1..n;
dvar int Heels_in_a_Handcart in 1..n;
dvar int The_Shoe_Palace     in 1..n;
dvar int Tootsies            in 1..n;
dvar int shops[1..n] = [Foot_Farm, Heels_in_a_Handcart, The_Shoe_Palace, Tootsies];


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(shoes,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);


}



constraints {

   allDifferent(shoes);
   allDifferent(shops);

   // 1. Harriet bought fuchsia flats at Heels in a Handcart.
   fuchsia_flats == Heels_in_a_Handcart;

   //  2. The store she visited just after buying her purple pumps was not Tootsies.
   purple_pumps + 1 != Tootsies;

   //  3. The Foot Farm was Harriet's second stop.
   Foot_Farm == 2;

   // 4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
   The_Shoe_Palace + 2 == suede_sandals;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("shoes :", t.shoes);
        writeln("shops :", t.shops);
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
