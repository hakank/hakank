/*

  Another set covering problem in Gecode.

  Problem from 
  Katta G. Murty: "Optimization Models for Decision Making", page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
 
  10 senators making a committee, where there must at least be one 
  representative from each group:
  group:        senators:
  southern      1 2 3 4 5
  northern      6 7 8 9 10
  liberals      2 3 8 9 10
  conservative  1 5 6 7
  democrats     3 4 5 6 7 9
  republicans   1 2 8 10

  The objective is to minimize the number of senators.


  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/set_covering3_model.mzn (model)
              http://www.hakank.org/minizinc/set_covering3.mzn (data)
  * Comet: http://www.hakank.org/comet/set_covering3.co
  * ECLiPSe: http://www.hakank.org/eclipse/set_covering3.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/set_covering3.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class SetCovering : public MinimizeScript {
protected:

  
  static const int num_groups = 6;
  static const int num_senators = 10;

  // cities: which columns to pick
  IntVarArray x;   
  IntVar numNeeded;

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  SetCovering(const SizeOptions& opt) 
  : 
    x(*this, num_senators, 0, 1),
    numNeeded(*this, 0, num_senators)
  {

    int _belongs[] =
      {
        1, 1, 1, 1, 1, 0, 0, 0, 0, 0,   // 1 southern
        0, 0, 0, 0, 0, 1, 1, 1, 1, 1,   // 2 northern
        0, 1, 1, 0, 0, 0, 0, 1, 1, 1,   // 3 liberals
        1, 0, 0, 0, 1, 1, 1, 0, 0, 0,   // 4 conservative
        0, 0, 1, 1, 1, 1, 1, 0, 1, 0,   // 5 democrats
        1, 1, 0, 0, 0, 0, 0, 1, 0, 1    // 6 republicans
      };
    IntArgs belongs(num_senators*num_groups, _belongs);

    for(int i = 0; i < num_groups; i++) {
      IntVarArgs tmp;
      for(int j = 0; j < num_senators; j++) {
        tmp << expr(*this, belongs[i*num_senators+j]*x[j]);
      }
      rel(*this, sum(tmp) >= 1);
    }

    rel(*this, sum(x) == numNeeded);

    if (opt.search() == SEARCH_DFS) {
      rel(*this, numNeeded == 2);
    }
    
    branch(*this, x, INT_VAR_SIZE_MAX(), INT_VAL_SPLIT_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "num needed: " << numNeeded << endl;
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  SetCovering(bool share, SetCovering& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    numNeeded.update(*this, share, s.numNeeded);
  }

  virtual IntVar cost(void) const {
    return numNeeded;
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SetCovering(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("SetCovering");
  opt.solutions(0);

  opt.search(SetCovering::SEARCH_BAB);
  opt.search(SetCovering::SEARCH_DFS, "dfs");
  opt.search(SetCovering::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  switch (opt.search()) {
    case SetCovering::SEARCH_DFS:
      MinimizeScript::run<SetCovering,DFS,SizeOptions>(opt); break;
    case SetCovering::SEARCH_BAB:
      MinimizeScript::run<SetCovering,BAB,SizeOptions>(opt); break;
    }

  return 0;

}


