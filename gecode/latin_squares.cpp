/*

  Latin squares in Gecode.


  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/latin_square.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/latin_squares.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/latin_squares.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;

void latin_square(Space& space, Matrix<IntVarArray> m, 
                  IntConLevel icl = ICL_DOM) {

  // we assume that the matrix is a square
  int n = m.width();
  for(int i = 0; i < n; i++) {
    distinct(space, m.row(i), icl);
    distinct(space, m.col(i), icl);
  }
  
}

int num_solutions; 

class LatinSquares : public Script {
protected:

  int n;          // size of the grid
  IntVarArray x;  // the grid

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    // SEARCH_BAB,     // Use branch and bound to optimize
    // SEARCH_RESTART, // Use restart to optimize
  };

  // Symmetry options
  enum {
    SYMMETRY_NONE,
    SYMMETRY_MIN    // use symmetry breaking
  };

  LatinSquares(const SizeOptions& opt) 
    : 
    n(opt.size()), 
    x(*this, n*n, 1, n) {

  
    // Matrix wrapper for the x grid
    Matrix<IntVarArray> m(x, n, n);

    latin_square(*this, m, opt.icl());

    // Symmetry breaking. 0 is upper left column
    if (opt.symmetry() == SYMMETRY_MIN) {
      rel(*this, x[0] == 1, opt.icl());
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_RANGE_MAX());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        os << x[i*n+j].val() << " ";
      }
      os << endl;
    }
    os << endl;
    num_solutions++;
  }

  // Constructor for cloning s
  LatinSquares(bool share, LatinSquares& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new LatinSquares(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("LatinSquares");

  num_solutions = 0;

  opt.solutions(0);
  opt.size(4); // n: the size of the problem

  opt.search(LatinSquares::SEARCH_DFS);
  opt.search(LatinSquares::SEARCH_DFS, "dfs");

  opt.symmetry(LatinSquares::SYMMETRY_NONE);
  opt.symmetry(LatinSquares::SYMMETRY_NONE, "none", "do not use symmetry");
  opt.symmetry(LatinSquares::SYMMETRY_MIN, "min", "minimum element first");

  opt.parse(argc,argv);

  switch (opt.search()) {
    case LatinSquares::SEARCH_DFS:
      Script::run<LatinSquares,DFS,SizeOptions>(opt); break;
  }

  cout << "num_solutions2:" << num_solutions << endl;

  return 0;
}


