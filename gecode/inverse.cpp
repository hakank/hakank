/*

  Global constraint inverse in Gecode.

  Decomposition of global constraint inverse.

  From MiniZinc globals.mzn
  """
  Constrains two arrays of int variables, 'f' and 'invf', to represent
  inverse functions.  All the values in each array must be within the 
  index set of the other array.
  """

  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/distribute.co
  * ECLiPSe: http://www.hakank.org/eclipse/distribute.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/distribute.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void inverse(Space& space, IntVarArray f, IntVarArray invf) {

  for(int i = 0; i < f.size(); i++) {
    for(int j = 0; j < invf.size(); j++) {
      rel(space, (j == f[i]) == (i == invf[j]));
    }
  }

}


class Inverse : public Script {
protected:

  // static const int n = 10;
  static const int n = 5;

  IntVarArray x;
  IntVarArray y;

public:

  Inverse(const Options& opt) 
  : 
    x(*this, n, 0, n-1),
    y(*this, n, 0, n-1)
  {

    // int _x[] = {10,1,6,7,4,5,8,3,2,9};
    // convert to 0-based
    int _x[] = {9,0,5,6,3,4,7,2,1,8};
    for(int i = 0; i < n; i++) {
      // rel(*this, x[i] == _x[i]);
    }
    
    inverse(*this, x, y);

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, y, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "y: " << y << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Inverse(bool share, Inverse& s) : Script(share,s) {
    x.update(*this, share, s.x);
    y.update(*this, share, s.y);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Inverse(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Inverse");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Inverse,DFS,Options>(opt);

  return 0;
}


