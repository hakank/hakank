/*

  Set partition, set covering in Gecode.

  Example from Lundgren, Rönnqvist, Värbrand "Optimeringslära", page 408.
  
  We want to minimize the cost of the alternatives which covers all the 
  objects, i.e. all objects must be choosen. The requirement is than an object 
  may be selected _exactly_ once.
 
  Alternative        Cost        Object
  1                  19           1,6
  2                  16           2,6,8
  3                  18           1,4,7
  4                  13           2,3,5
  5                  15           2,5
  6                  19           2,3
  7                  15           2,3,4
  8                  17           4,5,8
  9                  16           3,6,8
  10                 15           1,6,7
 
  The problem has a unique solution of z = 49 where alternatives 
  3, 5, and 9 is selected. 
 
  If we, however, allow that an object is selected more than one time, 
  then the solution is z = 45 (i.e. less cost than the first problem),
  and the alternatives 4, 8, and 10 is selected, where object 5 is 
  selected twice (alt. 4 and 8). It's an unique solution as well.
 

  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/set_covering4.mzn
  * Comet: http://www.hakank.org/comet/set_covering4.co
  * ECLiPSe: http://www.hakank.org/eclipse/set_covering4.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/set_covering4.pl

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

  
  static const int num_alternatives = 10;
  static const int num_objects = 8;

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
    x(*this, num_alternatives, 0, 1),
    z(*this, 0, 999999)
  {

    // costs per alternative
    int _costs[] = {19, 16, 18, 13, 15, 19, 15, 17, 16, 15};
    IntArgs costs(num_alternatives, _costs);

    // the alternatives and the objects they contain
    int _a[] = {
   // 1 2 3 4 5 6 7 8  the objects 
      1,0,0,0,0,1,0,0,  // alternative 1
      0,1,0,0,0,1,0,1,  // alternative 2
      1,0,0,1,0,0,1,0,  // alternative 3
      0,1,1,0,1,0,0,0,  // alternative 4
      0,1,0,0,1,0,0,0,  // alternative 5
      0,1,1,0,0,0,0,0,  // alternative 6
      0,1,1,1,0,0,0,0,  // alternative 7
      0,0,0,1,1,0,0,1,  // alternative 8
      0,0,1,0,0,1,0,1,  // alternative 9
      1,0,0,0,0,1,1,0,  // alternative 10
    };
    IntArgs a(num_alternatives*num_objects, _a);


    for(int j = 0; j < num_objects; j++) {
      IntVarArgs tmp; 
      for(int i = 0; i < num_alternatives; i++) {
        tmp << expr(*this, x[i]*a[i*num_objects+j]);
      }
      if (opt.size() == 0) {
        // set partition problem:
        // objects must be covered _exactly_ once
        rel(*this, sum(tmp) == 1);
      } else {
        // set covering problem
        // all objects must be covered _at least_ once
        rel(*this, sum(tmp) >= 1);
      }      
    }

    if (opt.search() == SEARCH_DFS) {
      if (opt.size() == 0) {
        rel(*this, z <= 49);
      } else {
        rel(*this, z <= 45);
      }
    }
    
    linear(*this, costs, x, IRT_EQ, z);

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

  // size = 0: set_covering
  // size = 1: set_partition
  opt.size(0);

  opt.search(SetCovering::SEARCH_BAB);
  opt.search(SetCovering::SEARCH_DFS, "dfs");
  opt.search(SetCovering::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (opt.size() == 0) {
    cout << "Set partition problem. "<< endl;
  } else {
    cout << "Set covering problem. "<< endl;
  }

  switch (opt.search()) {
    case SetCovering::SEARCH_DFS:
      MinimizeScript::run<SetCovering,DFS,SizeOptions>(opt); break;
    case SetCovering::SEARCH_BAB:
      MinimizeScript::run<SetCovering,BAB,SizeOptions>(opt); break;
    }

  return 0;

}


