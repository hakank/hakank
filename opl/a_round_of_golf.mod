/* 

  A Round of Golf puzzle (Dell Logic Puzzle) in OPL.

  From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
  """
  Title: A Round of Golf
  Author: Ellen K. Rodehorst
  Publication: Dell Favorite Logic Problems
  Issue: Summer, 2000
  Puzzle #: 9
  Stars: 1

  When the Sunny Hills Country Club golf course isn't in use by club members, 
  of course, it's open to the club's employees. Recently, Jack and three other 
  workers at the golf course got together on their day off to play a round of 
  eighteen holes of golf. 
  Afterward, all four, including Mr. Green, went to the clubhouse to total 
  their scorecards. Each man works at a different job (one is a short-order 
  cook), and each shot a different score in the game. No one scored below 
  70 or above 85 strokes. From the clues below, can you discover each man's 
  full name, job and golf score?
  
  1. Bill, who is not the maintenance man, plays golf often and had the lowest 
  score of the foursome.
  2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten 
  strokes more than the pro-shop clerk.
  3. In some order, Frank and the caddy scored four and seven more strokes than 
  Mr. Sands.
  4. Mr. Carter thought his score of 78 was one of his better games, even 
     though Frank's score  was lower.
  5. None of the four scored exactly 81 strokes.
  
  Determine: First Name - Last Name - Job - Score 
  """

  first_name:  [1 2 3 4]
  last_name :  [4 1 2 3]
  job       :  [2 1 4 3]
  score     :  [85 71 78 75]

  This model use the inverse constraint for the neater presentation:
  Jack Clubb maintenance_man score: 85
  Bill Sands cook score: 71
  Paul Carter caddy score: 78
  Frank Green clerk score: 75



  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;

int Jack  = 1;
int Bill  = 2;
int Paul  = 3;
int Frank = 4;
int first_name[1..n] = [Jack, Bill, Paul, Frank];
string fn_s[1..n] = ["Jack","Bill","Paul","Frank"];

// decision variables
// last names
dvar int Green in 1..n;
dvar int Clubb in 1..n;
dvar int Sands in 1..n;
dvar int Carter in 1..n;
dvar int last_name[1..n] = [Green, Clubb, Sands, Carter];
dvar int last_name_inv[1..n] in 1..n; // for presentation (via inverse)
string ln_s[1..n] = ["Green","Clubb","Sands","Carter"];

// jobs
dvar int cook in 1..n;
dvar int maintenance_man in 1..n;
dvar int clerk in 1..n;
dvar int caddy in 1..n;
dvar int job[1..n] = [cook, maintenance_man, clerk, caddy];
dvar int job_inv[1..n] in 1..n;  // for presentation (via inverse)
string j_s[1..n] = ["cook","maintenance_man","clerk","caddy"];

dvar int score[1..n] in 70..85;




execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Medium"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(last_name,
                        // variable
                        f.selectSmallest(f.domainMin()),
                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

}

constraints {

   allDifferent(last_name);
   allDifferent(job);
   allDifferent(score);

   // 1. Bill, who is not the maintenance man, plays golf often and had 
   // the lowest score of the foursome.
   Bill != maintenance_man;
   score[Bill] < score[Jack];
   score[Bill] < score[Paul];
   score[Bill] < score[Frank];
 
   // 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
   //    scored ten strokes more than the pro-shop clerk.
   Clubb != Paul;
   score[Clubb] == score[clerk] + 10;
   

   // 3. In some order, Frank and the caddy scored four and seven more 
   //    strokes than Mr. Sands.
   Frank != caddy;
   Frank != Sands;
   caddy != Sands;

    (score[Frank] == score[Sands] + 4 &&
     score[caddy] == score[Sands] + 7 )
    ||
    (score[Frank] == score[Sands] + 7 &&
     score[caddy] == score[Sands] + 4 );

    // 4. Mr. Carter thought his score of 78 was one of his better games, even 
    // though Frank's score was lower.
    Frank != Carter;
    score[Carter] == 78;
    score[Frank] < score[Carter];

   // 5. None of the four scored exactly 81 strokes.
   forall(i in 1..n) {
     score[i] != 81;
   }

   // For the presentation
   inverse(last_name, last_name_inv);
   inverse(job, job_inv);
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var t = thisOplModel;
      writeln("first_name: ", t.first_name);
      writeln("last_name : ", t.last_name);
      writeln("job       : ", t.job);
      writeln("score     : ", t.score);
      writeln();
      for(i = 1; i <= thisOplModel.n; i++) {
          writeln(t.fn_s[thisOplModel.first_name[i]], " ", 
                  t.ln_s[thisOplModel.last_name_inv[i]], " ", 
                  t.j_s[thisOplModel.job_inv[i]], 
                  " score: ", t.score[i]);
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
