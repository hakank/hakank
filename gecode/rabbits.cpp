/*

  Rabbits problem in Gecode.

  From Pascal Van Hentenryck "The OPL Optimization Programming Language",
  page 9.

  Compare with the following models:
  * Comet: http://www.hakank.org/comet/rabbits.co
  * ECLiPSe: http://www.hakank.org/eclipse/rabbits.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/rabbits.pl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;

class Rabbits : public Script {
protected:

  static const int n = 20;
  IntVar NbRabbits;
  IntVar NbPheasants;

public:

  Rabbits(const SizeOptions& opt) 
    : 
    NbRabbits(*this, 0, n),
    NbPheasants(*this, 0, n)
  {

    rel(*this, 20 == NbRabbits + NbPheasants);
    rel(*this, 56 == 4*NbRabbits + 2*NbPheasants);

    branch(*this, NbRabbits, INT_VAL_MIN());
    branch(*this, NbPheasants, INT_VAL_MIN());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    os << "NbRabbits: " << NbRabbits << endl;
    os << "NbPheasants: " << NbPheasants << endl;
  }

  // Constructor for cloning s
  Rabbits(bool share, Rabbits& s) : Script(share,s) {
    NbRabbits.update(*this, share, s.NbRabbits);
    NbPheasants.update(*this, share, s.NbPheasants);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Rabbits(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Rabbits");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Rabbits,DFS,SizeOptions>(opt);

  return 0;
}


