/*
  
  Simple coloring problem in Comet.

  From Pascal Van Hentenryck "The OPL Optimization Programming Language",
  page 7, 42.
  
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/map.mzn
  * Comet: http://www.hakank.org/comet/map.co
 

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/


 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class Coloring : public MinimizeScript {
protected:
  IntVarArray x;          // array of digits
  IntVar num_colors;      // difference to minimize
  static const int n = 6; // number of countries
public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  // Branching to use for model
  // (This was inspired by the Gecode example graph-coloring)
  enum {
    BRANCH_DEGREE,      // Choose variable with largest degree
    BRANCH_SIZE,        // Choose variable with smallest size
    BRANCH_SIZE_DEGREE  // Choose variable with smallest size/degree
  };


  // Actual model
  Coloring(const SizeOptions& opt)
    : 
    x(*this, n ,1, 10), 
    num_colors(*this, 1, 10) {


    // The connections of the countries
    // belgium, denmark, france, germany, netherlands, luxembourg
    int connections[] = { 
      0,   0,   1,   1,   1,   1,
      0,   0,   0,   1,   0,   0,
      1,   0,   0,   1,   1,   0,
      1,   1,   1,   0,   1,   1,
      1,   0,   1,   1,   0,   0,
      1,   0,   0,   1,   0,   0
    };

    for(int i = 0; i < n; i++) {
      for(int j = i+1; j < n; j++) {
        if (connections[i*n+j] > 0) {
          rel(*this, x[i] != x[j], opt.icl());
        }
      }
    }

    // how many colors is used?
    max(*this, x, num_colors, opt.icl());

    // some symmetry breaking
    rel(*this, x[0] == 1, opt.icl());


    // After the Gecode example graph-coloring.cpp
    if (opt.branching() == BRANCH_SIZE) {
      branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    } else if (opt.branching() == BRANCH_DEGREE) {
      branch(*this, x, tiebreak(INT_VAR_DEGREE_MAX(),INT_VAR_SIZE_MIN()),
             INT_VAL_MIN());
    } else {
      branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    }
    

  }

  // Return cost
  virtual IntVar cost(void) const {
    return num_colors;
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << x << " num_colors: " << num_colors << std::endl;
  }

  // Constructor for cloning \a s
  Coloring(bool share, Coloring& s)
    : MinimizeScript(share,s) {
    num_colors.update(*this, share, s.num_colors);
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Coloring(share,*this);
  }
};

/** 
 *
 * Main
 *
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("Coloring");
  opt.solutions(0);
  opt.icl(ICL_VAL);

  opt.search(Coloring::SEARCH_BAB);
  opt.search(Coloring::SEARCH_DFS, "dfs");
  opt.search(Coloring::SEARCH_BAB, "bab");

  // Branching: After the Gecode example graph-coloring.cpp
  opt.branching(Coloring::BRANCH_DEGREE);
  opt.branching(Coloring::BRANCH_DEGREE, "degree");
  opt.branching(Coloring::BRANCH_SIZE, "size");
  opt.branching(Coloring::BRANCH_SIZE_DEGREE, "sizedegree");

  opt.parse(argc,argv);
  switch (opt.search()) {
    case Coloring::SEARCH_DFS:
      MinimizeScript::run<Coloring,DFS,SizeOptions>(opt); break;
    case Coloring::SEARCH_BAB:
      MinimizeScript::run<Coloring,BAB,SizeOptions>(opt); break;
    }
  return 0;
}

