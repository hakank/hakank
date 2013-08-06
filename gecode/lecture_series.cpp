/*
  
  Lecture Series Puzzle (Dell Logic Puzzles) in Gecode.

  From http://brownbuffalo.sourceforge.net/LectureSeriesClues.html
  """
  Title: Lecture Series
  Author: Alex Knight
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 10
  Stars: 2

  Last week at school was made varied by a series of lectures, one
  each day 
   (Monday through Friday), 
  in the auditorium. None of the lectures was particularly 
  interesting 
     (on choosing a college, physical hygiene, modern art, nutrition, 
     and study habits), 
   but the students figured that anything that got them out of fourth 
   period was okay. The lecturers were 
       two women named Alice and Bernadette, and three men 
       named Charles, Duane, and Eddie; 
   last names were 
       Felicidad, Garber, Haller, Itakura, and Jeffreys. 
   Can you find each day's lecturer and subject?

  1. Alice lectured on Monday.
  2. Charles's lecture on physical hygiene wasn't given on Friday.
  3. Dietician Jeffreys gave the lecture on nutrition.
  4. A man gave the lecture on modern art.
  5. Ms. Itakura and the lecturer on proper study habits spoke on 
     consecutive days, in one order or the other.
  6. Haller gave a lecture sometime after Eddie did.
  7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
  
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/lecture_series.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/lecture_series.pl
  * ECLiPSe: http://www.hakank.org/eclipse/lecture_series.ecl

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


class LectureSeries : public Script {
protected:

  const static int n = 5;

  IntVarArray lectures;
  IntVarArray first_name;
  IntVarArray last_name;

public:

  LectureSeries(const Options& opt) 
    : 
    lectures(*this, n, 0, n-1),
    first_name(*this, n, 0, n-1),
    last_name(*this, n, 0, n-1)
  {

    enum {
      Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday
    };

    IntVar 
      choosing_a_college(lectures[0]), 
      physical_hygiene(lectures[1]), 
      modern_art(lectures[2]), 
      nutrition(lectures[3]), 
      study_habits(lectures[4]);


    IntVar
      Alice(first_name[0]), 
      Bernadette(first_name[1]), 
      Charles(first_name[2]), 
      Duane(first_name[3]), 
      Eddie(first_name[4]);


    IntVar
      Felicidad(last_name[0]),
      Garber(last_name[1]),
      Haller(last_name[2]),
      Itakura(last_name[3]),
      Jeffreys(last_name[4]);


    distinct(*this, lectures);
    distinct(*this, first_name);
    distinct(*this, last_name);

    
    // 1. Alice lectured on Monday.
    rel(*this, Alice == Monday);
    
    // 2. Charles's lecture on physical hygiene wasn't given on Friday.
    rel(*this,
        Charles == physical_hygiene && 
        Charles != Friday && 
        physical_hygiene != Friday 
        );
    
    // 3. Dietician Jeffreys gave the lecture on nutrition.
    rel(*this, Jeffreys == nutrition);
    
    // 4. A man gave the lecture on modern art.
    rel(*this, 
        modern_art == Charles || 
        modern_art == Duane || 
        modern_art == Eddie
        );
    
    // 5. Ms. Itakura and the lecturer on proper study habits spoke on 
    //    consecutive days, in one order or the other.
    rel(*this,
        (Itakura == Alice || Itakura == Bernadette) &&
        (abs(Itakura - study_habits) == 1) && 
        (Itakura != study_habits)
        );
    
    // 6. Haller gave a lecture sometime after Eddie did.
    rel(*this, Haller > Eddie);
    
    // 7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
    rel(*this,
        Duane == Felicidad &&
        Duane < modern_art
        );
    

    // branching
    branch(*this, lectures, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, first_name, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, last_name, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "lectures: " << lectures << endl;
    os << "first_name: " << first_name << endl;
    os << "last_name: " << last_name << endl;
  }


  // Constructor for cloning s
  LectureSeries(bool share, LectureSeries& s) : Script(share,s) {
    lectures.update(*this, share, s.lectures);
    first_name.update(*this, share, s.first_name);
    last_name.update(*this, share, s.last_name);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new LectureSeries(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("LectureSeries");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<LectureSeries,DFS,Options>(opt);
    
  return 0;
}


