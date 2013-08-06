/*

  Talisman Square in Gecode.

  See http://mathworld.wolfram.com/TalismanSquare.html
  """
  An n x n array  of the integers from 1 to n^2 such that the difference between 
  any one integer and its neighbor (horizontally, vertically, or 
  diagonally, without wrapping around) is greater than or equal to
  some value k is called a (n,k)-talisman square. 
  """



  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/talisman_square.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/talisman_square.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/talisman_square.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;
using std::setw;

using namespace Gecode;

class TalismanSquareOptions : public Options {
private:
  Driver::UnsignedIntOption _n_option; // n
  Driver::UnsignedIntOption _k_option; // k

public:

  TalismanSquareOptions(const char* n) 
    : Options(n),
      _n_option("-n",
                "n value",
                5),
      _k_option("-k",
                "k value",
                4)
  {
    add(_n_option);
    add(_k_option);
  }

  // Parse options from arguments argv (number is argc)
  void parse(int& argc, char* argv[]) {

    // Parse regular options
    Options::parse(argc,argv);    
  }

  // k
  unsigned int k(void) const  { return _k_option.value();  }

  // n to use
  unsigned int n(void) const  { return _n_option.value();  }


};

class TalismanSquare : public Script {
protected:

  int n;          // size of the grid
  int k;
  IntVarArray x;  // the grid

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    // SEARCH_BAB,     // Use branch and bound to optimize
  };

  // Symmetry options
  enum {
    SYMMETRY_NONE,
    SYMMETRY_MIN    // use symmetry breaking
  };

  TalismanSquare(const TalismanSquareOptions& opt) 
    : 
    n(opt.n()), 
    k(opt.k()), 
    x(*this, n*n, 1, n*n) {

    distinct(*this, x);

    for(int i = 1; i < n; i++) {
      for(int j = 1; j < n; j++) {
        rel(*this, abs(x[i*n+j]-x[(i-1)*n+j]) >= k);
        rel(*this, abs(x[i*n+j]-x[i*n+(j-1)]) >= k);
      }
    }

    for(int i = 0; i < n-1; i++) {
      for(int j = 0; j < n-1; j++) {
        rel(*this, abs(x[i*n+j]-x[(i+1)*n+j]) >= k);
        rel(*this, abs(x[i*n+j]-x[i*n+(j+1)]) >= k);
      }
    }

    // Symmetry breaking. 0 is upper left column
    if (opt.symmetry() == SYMMETRY_MIN) {
      rel(*this, x[0] == 1, opt.icl());
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        os << setw(3) << x[i*n+j] << " ";
      }
      os << endl;
    }
    os << endl;
  }

  // Constructor for cloning s
  TalismanSquare(bool share, TalismanSquare& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new TalismanSquare(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  TalismanSquareOptions opt("TalismanSquare");

  opt.solutions(0);

  opt.search(TalismanSquare::SEARCH_DFS);
  opt.search(TalismanSquare::SEARCH_DFS, "dfs");

  opt.symmetry(TalismanSquare::SYMMETRY_NONE);
  opt.symmetry(TalismanSquare::SYMMETRY_NONE, "none", "do not use symmetry");
  opt.symmetry(TalismanSquare::SYMMETRY_MIN, "min", "minimum element first");

  opt.parse(argc,argv);

  switch (opt.search()) {
    case TalismanSquare::SEARCH_DFS:
      Script::run<TalismanSquare,DFS,TalismanSquareOptions>(opt); break;
  }

  return 0;
}


