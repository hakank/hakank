/*
  
  Square root of WONDERFUL in Gecode.


  Compare with the following models:
  * Minizinc: http://www.hakank.org/minizinc/square_root_of_wonderful.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/square_root_of_wonderful.ecl

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


class Wonderful : public Script {
protected:

  static const int n = 9;

  IntVarArray x;
  IntVar WONDERFUL;

public:

  Wonderful(const Options& opt) 
    : 
    x(*this, n, 0, n),
    WONDERFUL(*this, 0, 999999999)
  {

    IntVar
      W(x[0]),
      O(x[1]),
      N(x[2]),
      D(x[3]),
      E(x[4]),
      R(x[5]),
      F(x[6]),
      U(x[7]),
      L(x[8]);

    IntVar OODDF(*this, 1, 99999);

    distinct(*this, x);

    rel(*this,
        WONDERFUL == 100000000*W + 10000000*O + 1000000*N + 100000*D + 10000*E + 1000*R +  100*F + 10*U + L &&
        OODDF == 10000*O + 1000*O + 100*D + 10*D + F &&
        OODDF*OODDF == WONDERFUL &&
        W >= 1 &&
        O >= 1
        );

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MAX());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;
    os << "WONDERFUL: " << WONDERFUL << endl;
    os << endl;

  }


  // Constructor for cloning s
  Wonderful(bool share, Wonderful& s) : Script(share,s) {
    x.update(*this, share, s.x);
    WONDERFUL.update(*this, share, s.WONDERFUL);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Wonderful(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Wonderful");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Wonderful,DFS,Options>(opt);
    
  return 0;
}


