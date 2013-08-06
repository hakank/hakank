/*
  
  Dinner problem in Gecode.

  http://www.sellsbrothers.com/spout/#The_Logic_of_Logic
  """
  My son came to me the other day and said, "Dad, I need help with a"
  "math problem." The problem went like this:

    * We're going out to dinner taking 1-6 grandparents, 1-10 parents and/or 1-40 children
    * Grandparents cost $3 for dinner, parents $2 and children $0.50
    * There must be 20 total people at dinner and it must cost $20
    * How many grandparents, parents and children are going to dinner?
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/dinner.mzn
  * ECLiPSE: http://www.hakank.org/eclipse/dinner.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/dinner.pl

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


class Dinner : public Script {
protected:

  IntVar Grandparents;
  IntVar Parents;
  IntVar Children;

public:

  Dinner(const Options& opt) 
    : 
    Grandparents(*this, 0, 100),
    Parents(*this, 0, 100),
    Children(*this, 0, 100)
  {

    // Grandparents * 3 + Parents * 2 + Children * 0.5 = 20
    // multiply with 2:
    rel(*this,
        Grandparents * 6 + Parents * 4 + Children * 1  == 40 &&
        Grandparents + Parents + Children == 20 &&
        // at least some of each group
        Grandparents > 0 &&
        Parents      > 0 &&
        Children     > 0
        );

    
    // branching
    branch(*this, Grandparents, INT_VAL_SPLIT_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Grandparents: " << Grandparents << endl;
    os << "Parents     : " << Parents << endl;
    os << "Children    : " << Children << endl;
  }


  // Constructor for cloning s
  Dinner(bool share, Dinner& s) : Script(share,s) {
    Grandparents.update(*this, share, s.Grandparents);
    Parents.update(*this, share, s.Parents);
    Children.update(*this, share, s.Children);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Dinner(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Dinner");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Dinner,DFS,Options>(opt);
    
  return 0;
}


