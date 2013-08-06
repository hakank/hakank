/*

  Mrs Timpkin's Age problem in Gecode.

  From 
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  Mrs Timpkin's Age    from "Amusements in Mathematics, Dudeney", number 43.
 
  When the Timpkinses married eighteen years ago, Timpkins was three
  times as old as his wife, and today he is just twice as old as she.
  How old is Mrs. Timpkin? 
  """

  Compare with my other models:
  * ECLiPSe : http://www.hakank.org/eclipse/timpkin.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/timpkin.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class Timpkin : public Script {
protected:

  static const int n = 2;

  IntVarArray x;

public:

  Timpkin(const Options& opt) 
  : 
    x(*this, n, 1, 100)
  {

    IntVar
      T(x[0]),
      W(x[1]);

    rel(*this, 
        T - 18 == 3 * (W - 18) &&
        T == 2 * W
        );

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Timpkin(bool share, Timpkin& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Timpkin(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Timpkin");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Timpkin,DFS,Options>(opt);

  return 0;
}


