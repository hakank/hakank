/*
  
  A Round of Golf puzzle (Dell Logic Puzzles) in Gecode.

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

  Compare with the F1 model: 
  http://www.f1compiler.com/samples/A 20Round 20of 20Golf.f1.html


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/a_round_of_golf.mzn
  * Comet   : http://www.hakank.org/comet/a_round_of_golf.co
  * ECLiPSe   : http://www.hakank.org/eclipse/a_round_of_golf.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class ARoundOfGolf : public Script {
protected:

  static const int n = 4;

  IntVarArray last_name;
  IntVarArray job;
  IntVarArray score;

public:

  ARoundOfGolf(const Options& opt) 
    : 
    last_name(*this, n, 0, n-1),
    job(*this, n, 0, n-1),
    score(*this, n, 70, 85)
  {

    enum {
      Jack,
      Bill,
      Paul,
      Frank
    };

    IntVar
      Green(last_name[0]),
      Clubb(last_name[1]),
      Sands(last_name[2]),
      Carter(last_name[3]);

    IntVar
      cook(job[0]),
      maintenance_man(job[1]),
      clerk(job[2]),
      caddy(job[3]);


    distinct(*this, last_name, opt.icl());
    distinct(*this, job, opt.icl());
    distinct(*this, score, opt.icl());

    // 1. Bill, who is not the maintenance man, plays golf often and had 
    // the lowest score of the foursome.
    rel(*this, 
        Bill != maintenance_man &&
        score[Bill] < score[Jack] &&
        score[Bill] < score[Paul] &&
        score[Bill] < score[Frank]
        );
 
    // 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
    //    scored ten strokes more than the pro-shop clerk.
    rel(*this,
        Clubb != Paul &&
        element(score, Clubb) == element(score, clerk) + 10);

   
    // 3. In some order, Frank and the caddy scored four and seven more 
    //    strokes than Mr. Sands.
    rel(*this, 
        Frank != caddy &&
        Frank != Sands &&
        caddy != Sands
        );

    rel(*this, 
        (score[Frank] == element(score,Sands) + 4 &&
         element(score,caddy) == element(score,Sands) + 7 )
        ||
        (score[Frank] == element(score,Sands) + 7 &&
         element(score,caddy) == element(score,Sands) + 4 
         )
        );


    // 4. Mr. Carter thought his score of 78 was one of his better games, even 
    //    though Frank's score was lower.
    rel(*this, Frank != Carter);
    rel(*this, element(score,Carter) == 78);
    rel(*this, score[Frank] < element(score,Carter));


    // 5. None of the four scored exactly 81 strokes.
    for(int i = 0; i < n; i++) {
      rel(*this, score[i] != 81);
    }

    // branching
    branch(*this, last_name, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, job, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, score, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "first_name: {0, 1, 2, 3}" << endl;
    os << "last_name : " << last_name << endl;
    os << "job       : " << job << endl;
    os << "score     : " << score << endl;
    os << endl;

  }


  // Constructor for cloning s
  ARoundOfGolf(bool share, ARoundOfGolf& s) : Script(share,s) {
    last_name.update(*this, share, s.last_name);
    job.update(*this, share, s.job);
    score.update(*this, share, s.score);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ARoundOfGolf(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("ARoundOfGolf");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<ARoundOfGolf,DFS,Options>(opt);
    
  return 0;
}


