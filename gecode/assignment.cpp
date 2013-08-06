/*

  Assignment problem in Gecode.

  Assignment problem from Winston "Operations Research, page 393f.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/assigment.mzn
  * Comet   : http://www.hakank.org/comet/assignment.co

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class Assignment : public MinimizeScript {
protected:

  static const int rows = 4;
  static const int cols = 5;

  // array of assignments (as rows x cols)
  IntVarArray x; 

  // the objective to minimize: the total cost
  IntVar total_cost;

  // for search all solutions with a total_cost
  // from the command line. Don't forget 
  //    -search dfs
  int total_cost_arg;

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  }
;

  Assignment(const SizeOptions& opt) 
    : 
    x(*this, cols*rows, 0, 1),
    total_cost(*this, 0, 100),
    total_cost_arg(opt.size())
  {

    // matrix version of x
    Matrix<IntVarArray> x_m(x, cols, rows);

    // hakank: I added the last column to make it more interesting.
    int _cost[] = 
      {
        14,  5,  8,  7, 15,
         2, 12,  6,  5,  3,
         7,  8,  3,  9,  7,
         2,  4,  6, 10,  1
      };

    IntArgs cost(cols*rows, _cost);

    // all rows must be assigned (exactly one)
    for(int r = 0; r < rows; r++) {
      linear(*this, x_m.row(r), IRT_EQ, 1, opt.icl());
    }

    // at most one assignement per column
    for(int c = 0; c < cols; c++) {
      linear(*this, x_m.col(c), IRT_LQ, 1, opt.icl());
    }

    // calculate total_cost (to minimize)
    linear(*this, cost, x, IRT_EQ, total_cost, opt.icl());

    // constrain with the argument from command line (if set)
    // don't forget -search dfs
    if (total_cost_arg) {
      rel(*this, total_cost == total_cost_arg, opt.icl());
    }

    // branching
    branch(*this, x, INT_VAR_DEGREE_MIN(), INT_VAL_MAX()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "total_cost: " << total_cost << std::endl;
    for(int r = 0; r < rows; r++) {
      for(int c = 0; c < cols; c++) {
        os << x[r*cols+c] << " ";
      }
      os << std::endl;
    }
    os << std::endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return total_cost;
  }


  // Constructor for cloning s
  Assignment(bool share, Assignment& s) : MinimizeScript(share,s),
                                          total_cost_arg(s.total_cost_arg) {
    x.update(*this, share, s.x);
    total_cost.update(*this, share, s.total_cost);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Assignment(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Assignment");
  opt.solutions(0);
  opt.iterations(20000);

  opt.search(Assignment::SEARCH_BAB);
  opt.search(Assignment::SEARCH_DFS, "dfs");
  opt.search(Assignment::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (opt.search() == Assignment::SEARCH_DFS) {
    Script::run<Assignment,DFS,SizeOptions>(opt);
  } else {
    MinimizeScript::run<Assignment,BAB,SizeOptions>(opt);
  }


  return 0;
}


