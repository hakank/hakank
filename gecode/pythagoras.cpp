/*

  Pythagoras problem in Gecode.


  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/pythagoras.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/pythagoras.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/pythagoras.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;

class Pythagoras : public Script {
protected:

  IntVarArray x;

public:

  Pythagoras(const SizeOptions& opt) 
    : 
    x(*this, 3, 0, Int::Limits::max)
  {

    IntVar
      A(x[0]),
      B(x[1]),
      C(x[2]);

    rel(*this,
        A >= 1 && B >= 1 && C >= 1  &&
        A*A + B*B == C*C &&
        A < B && B < C
        );

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    os << x[0] << "^2 + " << x[1] << "^2 = " << x[2] << "^2" << endl;
  }

  // Constructor for cloning s
  Pythagoras(bool share, Pythagoras& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Pythagoras(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Pythagoras");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Pythagoras,DFS,SizeOptions>(opt);

  return 0;
}


