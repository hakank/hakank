/*
  
  Recreational mathematics in Gecode.

  Problem 1.2 from 
  Averbach & Chein "Problem Solving Through Recreational Mathematics",
  page 2:
  """
  Ms X, Ms Y, and Ms Z - and American woman, and Englishwoman, and a 
  Frenchwoman, but not neccessarily in that order, were seated around 
  a circular table, playing a game of Hearts.
  Each passed three cards to the person on her right.
  Ms Y passed three hearts to the American, 
  Ms X passed the queen of spades and two diamonds to the person 
  who passed her cards to the Frenchwoman
 
  Who was the American? The Englishwoman? The Frenchwoman?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/averbach_1.2.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/averbach_1.2.pl
  * ECLiPSe: http://www.hakank.org/eclipse/averbach_1.2.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

void rightTo(Space& space, IntVar x, IntVar y) {
  rel(space, (x == y + 1) || (x == y -2) );
}

void leftTo(Space& space, IntVar x, IntVar y) {
  rightTo(space, y,x);
}


class Averbach : public Script {
protected:

  const static int n = 5;
  IntVarArray names;
  IntVarArray profession;


  IntVar American;
  IntVar English;
  IntVar French;

  IntVarArray xtable;

public:

  Averbach(const Options& opt) 
    : 
    American(*this, 1, 3),
    English(*this, 1, 3),
    French(*this, 1, 3),
    xtable(*this, 3, 1, 3)
  {

    IntVar 
      X(xtable[0]),
      Y(xtable[1]),
      Z(xtable[2]);

    IntVarArray ytable(*this, 3, 1, 3);
    ytable[0] = American;
    ytable[1] = English;
    ytable[2] = French;
    
    distinct(*this, xtable);
    distinct(*this, ytable);
    
    rightTo(*this, Y, American);
    leftTo(*this, X, French);

    // symmetry breaking
    rel(*this, X == 1);

    // branching
    branch(*this, xtable, INT_VAR_SIZE_MIN(), INT_VAL_MIN());


  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "xtable: " << xtable << endl;
    os << "American: " << American << endl;
    os << "English: " << English << endl;
    os << "French: " << French << endl;
    os << endl;
  }


  // Constructor for cloning s
  Averbach(bool share, Averbach& s) : Script(share,s) {
    xtable.update(*this, share, s.xtable);
    American.update(*this, share, s.American);
    English.update(*this, share, s.English);
    French.update(*this, share, s.French);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Averbach(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Averbach");

  opt.solutions(0);
  opt.icl(ICL_BND);

  opt.parse(argc,argv);

  Script::run<Averbach,DFS,Options>(opt);
    
  return 0;
}


