/*
 
  Coins puzzle in MiniZinc.
  
  Problem from 
  Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
 
  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one should 
  place coins in such a way that the following conditions are fulfilled:
     1. In each row exactly 14 coins must be placed.
     2. In each column exactly 14 coins must be placed.
     3. The sum of the quadratic horizontal distance from the main diagonal 
        of all cells
        containing a coin must be as small as possible.
     4. In each cell at most one coin can be placed.
  The description says to place 14x31 = 434 coins on the chessboard each row 
  containing 14 coins and each column also containing 14 coins.
  """
 
  Cf the LPL model:
  http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin
  

  See my other models:
  MiniZinc: http://www.hakank.org/minizinc/coins_grid.mzn
  Comet: http://www.hakank.org/comet/coins_grid.co
  JaCoP: http://www.hakank.org/JaCoP/CoinsGrid.java
  Choco: http://www.hakank.org/choco/CoinsGrid.java
  Gecode/R: http://www.hakank.org/gecode_r/coins_grid.rb


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class CoinsGrid : public MinimizeScript {
protected:

  IntVarArray x; // the array version of the grid
  IntVar z;      // distance to minimize
  static const int n = 7; // 31;
  static const int c = 3; // 14;
public:

  // search engines
  enum {
    SEARCH_BAB,
  }
;

  CoinsGrid(const Options& opt) 
    : 
    x(*this, n*n, 0, 1),
    z(*this, 0, 100000000)
  {
    
    // Matrix-wrapper for the square
    Matrix<IntVarArray> m(x, n, n);

    // rows
    for (int i = 0; i < n; i++) {
      linear(*this, m.row(i), IRT_EQ, c, opt.icl());
      linear(*this, m.col(i), IRT_EQ, c, opt.icl());
    }

    // quadratic horizontal distance
    IntVarArray dist(*this, n*n, 0, 10000000);
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        int t = abs(i-j);
        rel(*this, dist[i*n+j] == m(i,j)*t*t);
      }
    }
    linear(*this, dist, IRT_EQ, z, opt.icl());

    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());
    // branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 
    // branch(*this, x, INT_VAR_REGRET_MIN()_MIN(), INT_VAL_SPLIT_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "z: " << z << std::endl;
    // Matrix-wrapper for the square
    Matrix<IntVarArray> m(x, n, n);
    // os << m << std::endl;
    for (int i = 0; i < n; i++) {
      os << "\t";
      for (int j = 0; j < n; j++) {
        // os.width(1);
        os << m(i,j) << "";
      }
      os << std::endl;
    }
    os << std::endl;


  }

  // Return cost
  virtual IntVar cost(void) const {
    return z;
  }

  // Constructor for cloning s
  CoinsGrid(bool share, CoinsGrid& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CoinsGrid(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("CoinsGrid");
  opt.solutions(0);
  // opt.iterations(20000);
  opt.search(CoinsGrid::SEARCH_BAB);
  opt.search(CoinsGrid::SEARCH_BAB, "bab");


  opt.parse(argc,argv);
  MinimizeScript::run<CoinsGrid,BAB,Options>(opt);

  return 0;
}


