/*

  Remainder problem in Gecode.

  """
  11.  Is there a number which when divided by 3 gives a remainder of 1;
  when divided by 4, gives a remainder of 2; when divided by 5, gives a
  remainder of 3; and when divided by 6, gives a remainder of 4?
  (Kordemsky)
  """

  Compare with the following models:
  * ECLiPSe: http://www.hakank.org/eclipse/remainders.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/remainders.pl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;

class Remainders : public Script {
protected:

  IntVarArray x;

public:

  Remainders(const SizeOptions& opt) 
    : 
    x(*this, 5, 1, 10000)
  {

    IntVar
      X(x[0]),
      A(x[1]),
      B(x[2]),
      C(x[3]),
      D(x[4]);

    rel(*this,
        X == A*3 + 1 &&
        X == B*4 + 2 &&
        X == C*5 + 3 &&
        X == D*6 + 4
        );


    branch(*this, x, INT_VAR_SIZE_MAX(), INT_VAL_MIN());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;
  }

  // Constructor for cloning s
  Remainders(bool share, Remainders& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Remainders(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Remainders");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Remainders,DFS,SizeOptions>(opt);

  return 0;
}


