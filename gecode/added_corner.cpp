/*
  
  Added corner puzzle in Gecode.

 
  Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
  """
  This puzzle requires that you enter the digits 1 through 8 in the circles 
  and squares (one digit in each figure) so that the number in each square 
  is equal to the sum on the numbers in the circles which  adjoin it.  
  ...
  
     C F C
     F   F
     C F C
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/added_corner.mzn
  * Comet: http://www.hakank.org/comet/added_corner.co
  * SICStus Prolog: http://www.hakank.org/sicstus/added_corner.pl
  * ECLiPSE: http://www.hakank.org/eclipse/added_corner.ecl


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


class AddedCorner : public Script {
protected:

  static const int n = 8;

  IntVarArray x;

public:

  AddedCorner(const Options& opt) 
    : 
    x(*this, n, 1, 8)
  {

    IntVar
      A(x[0]),
      B(x[1]),
      C(x[2]),
      D(x[3]),
      E(x[4]),
      F(x[5]),
      G(x[6]),
      H(x[7]);

    distinct(*this, x);

    rel(*this, 
        B == A + C  &&
        D == A + F  &&
        E == C + H  &&
        G == F + H
        );

    // branching
    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << x[0] << "  " << x[1] << "  " << x[2] << endl;
    os << x[3] << "  " << " " << "  " << x[4] << endl;
    os << x[5] << "  " << x[6] << "  " << x[7] << endl;
    os << endl;

  }


  // Constructor for cloning s
  AddedCorner(bool share, AddedCorner& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AddedCorner(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("AddedCorner");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<AddedCorner,DFS,Options>(opt);
    
  return 0;
}


