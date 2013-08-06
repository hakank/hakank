/*

  Set covering employment in Gecode.

  Problem from
  http://mathworld.wolfram.com/SetCoveringDeployment.html


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering_deployment.mzn
  * Comet   : http://www.hakank.org/comet/set_covering_deployment.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

const std::string Countries[] = {"alexandria", "asia_minor", "britain", "byzantium", "gaul", "iberia", "rome", "tunis"};

class SetCoveringDeployment : public MinimizeScript {
protected:

  static const int n = 8;  // number of countries
  IntVar num_armies;         // number of armies (to minimize)

  IntVarArray X;             // the first army
  IntVarArray Y;             // the second (reserve) army

  int num_armies_args;       // parameter number of armies from command line

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };


  SetCoveringDeployment(const SizeOptions& opt) 
    : 
    num_armies(*this, 0, n),
    X(*this, n, 0, 1),
    Y(*this, n, 0, 1),
    num_armies_args(opt.size())
  {


    // the incidence matrix   
    // See the map at 
    // http://mathworld.wolfram.com/SetCoveringDeployment.html
    int mat[] = 
      { 
       0,   1,   0,   1,   0,   0,   1,   1,
       1,   0,   0,   1,   0,   0,   0,   0,
       0,   0,   0,   0,   1,   1,   0,   0,
       1,   1,   0,   0,   0,   0,   1,   0,
       0,   0,   1,   0,   0,   1,   1,   0,
       0,   0,   1,   0,   1,   0,   1,   1,
       1,   0,   0,   1,   1,   1,   0,   1,
       1,   0,   0,   0,   0,   1,   1,   0
       };


    //
    // calculate num_armies
    //
    IntVarArray XY(*this, n, 0, n*n);
    for(int i = 0; i < n; i++) {
      rel(*this, X[i]+Y[i]==XY[i], opt.icl()); 
    }
    linear(*this, XY, IRT_EQ, num_armies, opt.icl());
    

    //
    // Constraint 1: There is always an army in a city (+ maybe a backup)
    //               Or rather: Is there a backup, there must be an an army
    for(int i = 0; i < n; i++ ) 
      rel(*this, X[i] >= Y[i], opt.icl());


    //
    // Constraint 2: There should always be an backup army near every city
    //
    for(int i = 0; i < n; i++) {
      IntVarArray y_tmp(*this, n, 0, n);
      for(int j = 0; j < n; j++) {
        if (mat[i*n+j] == 1) {
          rel(*this, y_tmp[j] == Y[j], opt.icl());
        } else {
          rel(*this, y_tmp[j] == 0, opt.icl());
        }
      }
      IntVar y_sum(*this, 0, n);
      linear(*this, y_tmp, IRT_EQ, y_sum, opt.icl());
      rel(*this, X[i] + y_sum  >= 1, opt.icl());
    }


    // Constraint 3 for full search
    // don't forget 
    //   -search dfs
    if (num_armies_args) {
      rel(*this, num_armies <= num_armies_args, opt.icl());
    }


    branch(*this, X, INT_VAR_DEGREE_MAX(), INT_VAL_MIN()); 
    branch(*this, Y, INT_VAR_DEGREE_MAX(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {

    os << "num_armies: " << num_armies << std::endl;
    os << "X: " << X << std::endl;
    os << "Y: " << Y << std::endl;
    os << std::endl;

  }

  // Return cost
  virtual IntVar cost(void) const {
    return num_armies;
  }


  // Constructor for cloning s
  SetCoveringDeployment(bool share, SetCoveringDeployment& s) : MinimizeScript(share,s), 
                                                                num_armies_args(s.num_armies_args) {
    X.update(*this, share, s.X);
    Y.update(*this, share, s.Y);
    num_armies.update(*this, share, s.num_armies);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SetCoveringDeployment(share,*this);
  }

};


int
main(int argc, char* argv[]) {
  SizeOptions opt("SetCoveringDeployment");

  opt.solutions(0);
  opt.search(SetCoveringDeployment::SEARCH_BAB);
  opt.search(SetCoveringDeployment::SEARCH_DFS, "dfs");
  opt.search(SetCoveringDeployment::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  switch (opt.search()) {
    case SetCoveringDeployment::SEARCH_DFS:
      MinimizeScript::run<SetCoveringDeployment,DFS,SizeOptions>(opt); break;
    case SetCoveringDeployment::SEARCH_BAB:
      MinimizeScript::run<SetCoveringDeployment,BAB,SizeOptions>(opt); break;
    }
  return 0;
}


