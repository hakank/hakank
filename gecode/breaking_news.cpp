/*
  
  Breaking news puzzle (Dell Logic Puzzles) in Gecode.

  From http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
  """
  Title: Breaking News
  Author: Faith Johnson
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1

  The Daily Galaxy sent its four best reporters 
      (Corey, Jimmy, Lois, and Perry) 
  to different locations 
      (Bayonne, New Hope, Port Charles, and South Amboy) 
  to cover four breaking news events 
      (30-pound baby, blimp launching, skyscraper dedication, and 
       beached whale). 
  Their editor is trying to remember where each of the reporters is. 
  Can you match the name of each reporter with the place he or she 
  was sent, and the event that each covered?

  1. The 30-pound baby wasn't born in South Amboy or New Hope.
  2. Jimmy didn't go to Port Charles.
  3. The blimp launching and the skyscraper dedication were covered, 
     in some order, by Lois and the reporter who was sent to Port Charles.
  4. South Amboy was not the site of either the beached whale or the 
     skyscraper dedication.
  5. Bayonne is either the place that Corey went or the place where 
     the whale was beached, or both.

  Determine: Reporter -- Location -- Story
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/breaking_news.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/breaking_news.pl
  * ECLiPSe: http://www.hakank.org/eclipse/breaking_news.ecl

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


class BreakingNews : public Script {
protected:

  static const int n = 4;

  IntVarArray locations;
  IntVarArray events;

public:

  BreakingNews(const Options& opt) 
    : 
    locations(*this, n, 0, n-1),
    events(*this, n, 0, n-1)
  {

    enum {
      Corey,
      Jimmy,
      Lois,
      Perry
    };

    IntVar
      Bayonne(locations[0]), 
      New_Hope(locations[1]), 
      Port_Charles(locations[2]), 
      South_Amboy(locations[3]);

    IntVar
      baby(events[0]),
      blimp(events[1]),
      skyscraper(events[2]),
      whale(events[3]);


    distinct(*this, locations, opt.icl());
    distinct(*this, events, opt.icl());

    // 1. The 30-pound baby wasn't born in South Amboy or New Hope.
    rel(*this,
        baby != South_Amboy &&
        baby != New_Hope
        );
 
    // 2. Jimmy didn't go to Port Charles.
    rel(*this, Jimmy != Port_Charles);

    // 3. The blimp launching and the skyscraper dedication were covered, 
    //    in some order, by Lois and the reporter who was sent to 
    //    Port Charles.
    rel(*this, 
        (
         (blimp == Lois && skyscraper == Port_Charles)
         ||
         (skyscraper == Lois && blimp == Port_Charles)
         )
        );

    // 4. South Amboy was not the site of either the beached whale or the 
    //    skyscraper  dedication.
    rel(*this, 
        South_Amboy != whale &&
        South_Amboy != skyscraper
        );

    // 5. Bayonne is either the place that Corey went or the place where 
    //    the whale was beached, or both.
    rel(*this, 
        ( (Bayonne == Corey) || (Bayonne == whale))
        );

    // branching
    branch(*this, locations, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, events, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "locations: " << locations << endl;
    os << "events : " << events << endl;
    os << endl;

  }


  // Constructor for cloning s
  BreakingNews(bool share, BreakingNews& s) : Script(share,s) {
    locations.update(*this, share, s.locations);
    events.update(*this, share, s.events);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BreakingNews(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("BreakingNews");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<BreakingNews,DFS,Options>(opt);
    
  return 0;
}


