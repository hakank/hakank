/*

  Seesway problem in Gecode.

  From Marriott & Stuckey "Programming with Constraints", page 257.

  Balancing on a seesaw.


  Compare with my other models:
  * MiniZinc: http://www.hakank.org/minizinc/stuckey_seesaw.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/stuckey_seesaw.pl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

// helper predicate
void apart(Space& space, IntVar x, IntVar y, int n) {
  rel(space, x >= y + n || y >= x + n);
}


class Seesaw : public Script {
protected:

  static const int n = 3;

  IntVarArray LD;

public:

  Seesaw(const Options& opt) 
  : 
    LD(*this, n, -5, 5)
  {

    IntVar 
      Liz(LD[0]),
      Fi(LD[1]),
      Sara(LD[2]);

    rel(*this, 9 * Liz + 8 * Fi + 4 * Sara == 0);
    apart(*this, Liz, Fi, 3);
    apart(*this, Liz, Sara, 3);
    apart(*this, Sara, Fi, 3);

    // symmetry breaking
    rel(*this, Sara >= 0);


    branch(*this, LD, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << LD << std::endl;
  }

  // Constructor for cloning s
  Seesaw(bool share, Seesaw& s) : Script(share,s) {
    LD.update(*this, share, s.LD);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Seesaw(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Seesaw");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Seesaw,DFS,Options>(opt);

  return 0;
}


