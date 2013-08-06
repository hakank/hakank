/*
  
  Circling the squares problem in Gecode.

  From http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/#circling
  """
  Circling the Squares from "Amusements in Mathematics, Dudeney",
  number 43.

  The puzzle is to place a different number in each of the ten squares
  so that the sum of the squares of any two adjacent numbers shall be
  equal to the sum of the squares of the two numbers diametrically
  opposite to them. The four numbers placed, as examples, must stand as
  they are. Fractions are not allowed, and no number need contain more
  than two figures. 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/circling_squares.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/circling_squares.pl
  * ECLiPSe: http://www.hakank.org/eclipse/circling_squares.ecl

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


// Predicate to simplify the constraint section
void s(Space& space, IntVar X1, IntVar X2, IntVar Y1, IntVar Y2) {
  rel(space, X1*X1 + X2*X2  == Y1*Y1 + Y2*Y2);
}



class CirclingSquares : public Script {
protected:

  static const int n = 10;

  IntVarArray LD;


public:

  CirclingSquares(const Options& opt) 
    : 
    LD(*this, n, 1, 99)
  {


    IntVar
      A(LD[0]), 
      B(LD[1]), 
      C(LD[2]), 
      D(LD[3]), 
      E(LD[4]), 
      F(LD[5]), 
      G(LD[6]), 
      H(LD[7]), 
      I(LD[8]), 
      // J(LD[9]), 
      K(LD[9]);


    distinct(*this, LD, opt.icl());

    rel(*this, 
        A == 16 &&
        B == 2 &&
        F == 8 &&
        G == 14
        );


    s(*this, A, B, F, G);
    s(*this, B, C, G, H);
    s(*this, C, D, H, I);
    s(*this, D, E, I, K);
    s(*this, E, F, K, A);

    // branching
    branch(*this, LD, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "LD: " << LD << endl;
    os << endl;

  }


  // Constructor for cloning s
  CirclingSquares(bool share, CirclingSquares& s) : Script(share,s) {
    LD.update(*this, share, s.LD);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CirclingSquares(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("CirclingSquares");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<CirclingSquares,DFS,Options>(opt);
    
  return 0;
}


