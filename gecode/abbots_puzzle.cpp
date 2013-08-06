/*
  
  Abbot's Puzzle in Gecode.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  The Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.

  If 100 bushels of corn were distributed among 100 people in such a
  manner that each man received three bushels, each woman two, and each
  child half a bushel, how many men, women, and children were there?

  Dudeney added the condition that there are five times as many women as
  men. That way, the solution becomes unique (otherwise, there are seven
  solutions). 
  """

  Compare with the following:
  * MiniZinc: http://www.hakank.org/minizinc/abpuzzle.mzn
  * ECLiPSe   : http://www.hakank.org/eclipse/abbots_puzzle.ecl

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


class AbbotsPuzzle : public Script {
protected:

  static const int n = 3;

  IntVarArray x;

public:

  AbbotsPuzzle(const Options& opt) 
    : 
    x(*this, n, 0, 1000)
  {

    // multiply with 2 for the integer solution
    int _amount[] = {6,4,1};
    IntArgs amount(3, _amount);
    linear(*this, amount, x, IRT_EQ, 200, opt.icl());
    rel(*this, sum(x) == 100);
    rel(*this, x[1] == 5*x[0]);

    // branching
    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x : " << x << endl;
    os << endl;

  }


  // Constructor for cloning s
  AbbotsPuzzle(bool share, AbbotsPuzzle& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AbbotsPuzzle(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("AbbotsPuzzle");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<AbbotsPuzzle,DFS,Options>(opt);
    
  return 0;
}


