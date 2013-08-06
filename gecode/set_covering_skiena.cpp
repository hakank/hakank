/*

  Set covering problem in Gecode.

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the 
  universal set U = {1,...,n}.
  
  Problem: What is the smallest subset of subsets T subset S such 
  that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.


  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/set_covering_skiena.mzn
  * Comet: http://www.hakank.org/comet/set_covering_skiena.co
  * ECLiPSe: http://www.hakank.org/eclipse/set_covering_skiena.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/set_covering_skiena.pl

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

  
  static const int num_sets = 7;
  static const int num_elements = 12;

  int min_option;

  IntVarArray x;   

  IntVar z;
  IntVar tot_elements;

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  SetCovering(const SizeOptions& opt) 
  : 
    x(*this, num_sets, 0, 1),
    z(*this, 0, num_sets),
    tot_elements(*this, 0, num_elements*num_sets)
  {

    if (opt.size() == 0) {
      min_option = 0;
    } else {
      min_option = 1;
    }

    int _belongs[] = {
  //  1 2 3 4 5 6 7 8 9 0 1 2  elements
      1,1,0,0,0,0,0,0,0,0,0,0, // Set 1
      0,1,0,0,0,0,0,1,0,0,0,0, //     2
      0,0,0,0,1,1,0,0,0,0,0,0, //     3
      0,0,0,0,0,1,1,0,0,1,1,0, //     4
      0,0,0,0,0,0,0,0,1,1,0,0, //     5
      1,1,1,0,1,0,0,0,1,1,1,0, //     6
      0,0,1,1,0,0,1,1,0,0,1,1, //     7
    };
    IntArgs belongs(num_sets*num_elements, _belongs);

    // total number of sets used
    for(int j = 0; j < num_elements; j++) {
      IntVarArgs tmp;
      for(int i = 0; i < num_sets; i++) {
        tmp << expr(*this, belongs[i*num_elements+j]*x[i]);
      }
      rel(*this, sum(tmp) >= 1);
    }

    // total number of elements used
    IntVarArgs tot;
    for(int i = 0; i < num_sets; i++) {
      for(int j = 0; j < num_elements; j++) {
        tot << expr(*this, x[i]*belongs[i*num_elements+j]);
      }
    }
    rel(*this, tot_elements == sum(tot));

    // optimal solutions for -search dfs and -search lds
    if (opt.search() == SEARCH_DFS) {
      if (opt.size() == 0) {
        // the number of sets
        rel(*this, z <= 3);
      } else {
        // the number of elements
        rel(*this, tot_elements <= 12);
      }
    }

    rel(*this, z == sum(x));

    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "tot_elements: " << tot_elements << std::endl;
    os << "z: " << z << std::endl;
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  SetCovering(bool share, SetCovering& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
    tot_elements.update(*this, share, s.tot_elements);
  }

  virtual IntVar cost(void) const {
    if (min_option == 0) {
      return z;
    } else {
      return tot_elements;
    }
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

  opt.size(0);

  opt.parse(argc,argv);

  if (opt.size() == 0) {
    cout << "Minimizing z (total number of sets)." << endl;
  } else {
    cout << "Minimizing tot_elements (total number of elements)." << endl;
  }

  switch (opt.search()) {
    case SetCovering::SEARCH_DFS:
      MinimizeScript::run<SetCovering,DFS,SizeOptions>(opt); break;
    case SetCovering::SEARCH_BAB:
      MinimizeScript::run<SetCovering,BAB,SizeOptions>(opt); break;
    }

  return 0;

}


