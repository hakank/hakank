/*

  Global constraint among in Gecode.

  Decomposition of global constraint among:
  Requires exactly 'n' variables in 'x' to take one of the values in 'v'.

  See Global constraint catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong.html

  Compare with my other models:
  * Comet: http://www.hakank.org/comet/among.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/among.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

void among(Space& space, IntVarArray x, SetVar ss, IntVar a) {
  int n = x.size();

  BoolVarArgs b(space, n, 0, 1);
  for(int i = 0; i < n; i++) {
    rel(space, ss, SRT_SUP, x[i], b[i]);
  }
  rel(space, sum(b) == a);

}

class Among : public Script {
protected:

  static const int n = 5;

  IntVarArray x;
  IntVar a;

public:

  Among(const Options& opt) 
  : 
    x(*this, n, 1, 8),
    a(*this,  1, 8)
  {

    // int _s[] = {1,6,8};
    IntSet s( IntArgs() << 1 << 6 << 8);
    SetVar ss(*this, s,s);

    among(*this, x, ss, a);

    rel(*this, a == 3);

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "a: " << a << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Among(bool share, Among& s) : Script(share,s) {
    x.update(*this, share, s.x);
    a.update(*this, share, s.a);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Among(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Among");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Among,DFS,Options>(opt);

  return 0;
}


