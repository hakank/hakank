/*

  Set covering problem in Gecode.

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  AMPL model: http://taha.ineg.uark.edu/setcover.txt

  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/set_covering2.mzn
  * Comet: http://www.hakank.org/comet/set_covering2.co
  * ECLiPSe: http://www.hakank.org/eclipse/set_covering2.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/set_covering2.pl

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

  
  static const int n = 8;
  static const int num_corners = 11;

  // cities: which columns to pick
  IntVarArray x;   
  IntVar z;

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  SetCovering(const SizeOptions& opt) 
  : 
    x(*this, n, 0, 1),
    z(*this, 0, n)
  {

    // corners of each street (1-based)
    int _corners[] =
      {
        1,2,
        2,3,
        4,5,
        7,8,
        6,7,
        2,6,
        1,6,
        4,7,
        2,4,
        5,8,
        3,5
      };
    IntSet corners[num_corners];
    for(int i = 0; i < num_corners; i++) {
      int c1 = _corners[i*2+0]-1; // 0-based
      int c2 = _corners[i*2+1]-1;
      IntArgs tmp;
      tmp << c1;
      tmp << c2;
      corners[i] = IntSet(tmp);
    }

    // check that all the streets are covered
    for(int i = 0; i < num_corners; i++) {
      IntVarArgs tmp;
      for(int j = 0; j < n; j++) {
        if (corners[i].in(j)) {
          tmp << expr(*this, x[j]);
        }
      }
      rel(*this, sum(tmp) >= 1);
    }

    rel(*this, z == sum(x));

    // show all optimal solutions if
    //    -search dfs
    // or
    //    -search lds
    if (opt.search() == SEARCH_DFS) {
      rel(*this, z == 4);
    }
    
    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "z: " << z << std::endl;
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  SetCovering(bool share, SetCovering& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
  }

  virtual IntVar cost(void) const {
    return z;
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


