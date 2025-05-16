/* 

  Babysitting puzzle (Dell Logic Puzzles) in OPL.

  Problem from http://brownbuffalo.sourceforge.net/BabysittingClues.html
  """
  Title: Babysitting
  Author: Scott Marley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Each weekday, Bonnie takes care of five of the neighbors' children. 
  The children's names are Keith, Libby, Margo, Nora, and Otto; last names 
  are Fell, Gant, Hall, Ivey, and Jule. Each is a different number of years 
  old, from two to six. Can you find each child's full name and age?

  1. One child is named Libby Jule.
  2. Keith is one year older than the Ivey child, who is one year older 
     than Nora.
  3. The Fell child is three years older than Margo.
  4. Otto is twice as many years old as the Hall child.

  Determine: First name - Last name - Age 
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 5;
range r = 1..n;

int Keith = 1;
int Libby = 2;
int Margo = 3;
int Nora  = 4;
int Otto  = 5;
int first[1..n] = [Keith, Libby, Margo, Nora, Otto];
string first_s[1..n] = ["Keith", "Libby", "Margo", "Nora", "Otto"];

// decision variables

dvar int Fell in 1..n;
dvar int Gant in 1..n;
dvar int Hall in 1..n;
dvar int Ivey in 1..n;
dvar int Jule in 1..n;
dvar int last[1..n] = [Fell, Gant, Hall, Ivey, Jule];
dvar int last_inv[1..n] in 1..n; // for presentation (via inverse)
string last_s[1..n] = ["Fell", "Gant", "Hall", "Ivey", "Jule"];

dvar int age[r] in 2..6;


execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Medium"; // "Default", "Low", "Basic", "Medium", "Extended";

  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(last,
                        // variable
                        f.selectSmallest(f.domainMin()),
                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);



}

constraints {

   allDifferent(last);
   allDifferent(age);

   //  1. One child is named Libby Jule.
   Jule == Libby;

   //  2. Keith is one year older than the Ivey child, who is one 
   //     year older than Nora.
   age[Keith] == age[Ivey] + 1;
   Keith != Ivey;
   
   age[Ivey] == age[Nora] + 1;
   Ivey != Nora;

   //  3. The Fell child is three years older than Margo.
   age[Fell] == age[Margo] + 3;
   Fell != Margo;

   //  4. Otto is twice as many years old as the Hall child.
   age[Otto] == age[Hall]*2;
   Otto != Hall;

   // For the presentation
   inverse(last, last_inv);

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var t = thisOplModel;
      writeln("first: ", t.first);
      writeln("last : ", t.last);
      writeln("age  : ", t.age);
      writeln();
      for(i = 1; i <= thisOplModel.n; i++) {
          writeln(t.first_s[thisOplModel.first[i]], " ", 
                  t.last_s[thisOplModel.last_inv[i]], " ", 
                  " age: ", t.age[i]);
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
