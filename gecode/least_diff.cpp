/*

  Least diff problem in Gecode.

  The program solves the following problem:

  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once.


   Compare with the following models:
   * MiniZinc: http://www.hakank.org/minizinc/least_diff.mzn
   * Gecode/R: http://www.hakank.org/gecode_r/least_diff.rb
   * Choco: http://www.hakank.org/choco/LeastDiff.java
   * JaCoP: http://www.hakank.org/JaCoP/LeastDiff.java
   * Comet: http://www.hakank.org/comet/least_diff.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/


 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class LeastDiff : public MinimizeScript {
protected:

  IntVarArray x; // Array of digits
  IntVar Diff;   // Difference to minimize
public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  // Actual model
  LeastDiff(const SizeOptions& opt)
    : x(*this, 10,0,9), Diff(*this, 0,100000) {

    IntVar a(x[0]), b(x[1]), c(x[2]), d(x[3]), e(x[4]), 
           f(x[5]), g(x[6]), h(x[7]), i(x[8]), j(x[9]); 

    IntVar X(*this, 0, 10000000);
    IntVar Y(*this, 0, 10000000);

    distinct(*this, x, opt.icl());

    rel(*this, X == 10000*a + 1000*b + 100*c + 10*d + e, opt.icl());
    rel(*this, Y == 10000*f + 1000*g + 100*h + 10*i + j, opt.icl());
    // rel(*this, X > Y, opt.icl());
    rel(*this, Diff == X - Y, opt.icl());
    rel(*this, Diff >= 0, opt.icl());

    // Testing different branching strategies.
    // branch(*this, x, INT_VAR_NONE(), INT_VAL_SPLIT_MIN());
    branch(*this, x, INT_VAR_DEGREE_MIN(), INT_VAL_MIN());
    /*
    branch(*this, x, 
           static_cast<IntVarBranch>(opt.branching()),
           INT_VAL_MIN());
    */

  }

  // Return cost
  virtual IntVar cost(void) const {
    return Diff;
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    // os << "\tm[" << x << "] = " << Diff << std::endl;
    os << x[0] << x[1] << x[2] << x[3] << x[4] << " - ";
    os << x[5] << x[6] << x[7] << x[8] << x[9] << " = ";
    os << Diff << std::endl;
  }

  // Constructor for cloning \a s
  LeastDiff(bool share, LeastDiff& s)
    : MinimizeScript(share,s) {
    Diff.update(*this, share, s.Diff);
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new LeastDiff(share,*this);
  }
};

/** 
 *
 * Main
 *
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("LeastDiff");
  opt.solutions(0);
  opt.icl(ICL_VAL);

  opt.search(LeastDiff::SEARCH_BAB);
  opt.search(LeastDiff::SEARCH_DFS, "dfs");
  opt.search(LeastDiff::SEARCH_BAB, "bab");

  // The option names is from Gecode/FlatZinc mapping of the the MiniZinc
  // branch options.
  /*
  opt.branching(INT_VAR_SIZE_MIN); // first_fail is default
  opt.branching(INT_VAR_NONE, "input-order", "use VAR_NONE");
  opt.branching(INT_VAR_SIZE_MIN, "first-fail", "use VAR_SIZE_MIN");
  opt.branching(INT_VAR_SIZE_MAX, "anti-first-fail", "use VAR_SIZE_MAX");
  opt.branching(INT_VAR_MIN_MIN, "smallest", "use VAR_MIN_MIN");
  opt.branching(INT_VAR_MAX_MAX, "largest", "use VAR_MAX_MAX");
  opt.branching(INT_VAR_DEGREE_MAX, "occurrence", "use VAR_DEGREE_MAX");
  opt.branching(INT_VAR_REGRET_MIN_MAX, "max-regret", "use VAR_REGRET_MIN_MAX");
  opt.branching(INT_VAR_MIN_MAX, "var-min-max", "use VAR_MIN_MAX");
  */

  opt.parse(argc,argv);
  switch (opt.search()) {
    case LeastDiff::SEARCH_DFS:
      MinimizeScript::run<LeastDiff,DFS,SizeOptions>(opt); break;
    case LeastDiff::SEARCH_BAB:
      MinimizeScript::run<LeastDiff,BAB,SizeOptions>(opt); break;
    }
  return 0;
}

