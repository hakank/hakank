/*  

  Simple PERT model in Gecode.

  From Pascal van Hentenryck 
  "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
  http://citeseer.ist.psu.edu/300151.html

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/pert.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/pert.pl
  * ECLiPSe: http://www.hakank.org/eclipse/pert.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class Pert : public MinimizeScript {
protected:

  static const int max_time = 30; 
  static const int n = 11; 

  IntVarArray start;  // start times

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB     // Use branch and bound to optimize
  };

  Pert(const SizeOptions& opt) 
  :    
    start(*this, n, 0, max_time)
  {

    // times for the tasks
    int times[] = {7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1};

    // Dependencies between the tasks
    // Note: There is no Si
    // Note2: These are stated in base-1 notion,
    // fixed in the for loop below.
    int dependencies[] = 
      {
        2,1,  // Sb >= Sa + 7
        4,1,  // Sd >= Sa + 7
        3,2,  // Sc >= Sb + 3
        5,3,  // Se >= Sc + 1
        5,4,  // Se >= Sd + 8
        7,3,  // Sg >= Sc + 1
        7,4,  // Sg >= Sd + 8
        6,4,  // Sf >= Sd + 8
        6,3,  // Sf >= Sc + 1
        8,6,  // Sh >= Sf + 1
        9,8,  // Sj >= Sh + 3
        10,7, // Sk >= Sg + 1
        10,5, // Sk >= Se + 1
        10,9, // Sk >= Sj + 2
        11,10 // Send >= Sk + 1
      };
    int num_dependencies = 15;

    for(int i = 0; i < num_dependencies; i++) {
      int d1 = dependencies[i*2+0]-1;
      int d2 = dependencies[i*2+1]-1;
      rel(*this, start[d1] >= (start[d2] + times[d2]));
    }

    // Branching
    branch(*this, start, INT_VAR_SIZE_MIN(), INT_VAL_MIN());


  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "start: " << start << endl;
    os << "last start: " << start[n-1].val() << endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Pert(bool share, Pert& s) : MinimizeScript(share,s) {
    start.update(*this, share, s.start);
  }

  // Return cost
  virtual IntVar cost(void) const {
    return start[n-1];
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Pert(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Pert");
  opt.solutions(0);

  opt.search(Pert::SEARCH_BAB, "bab");
  opt.search(Pert::SEARCH_DFS, "dfs");
  opt.search(Pert::SEARCH_BAB, "bab");
 
  opt.parse(argc,argv);

  switch (opt.search()) {
    case Pert::SEARCH_DFS:
      MinimizeScript::run<Pert,DFS,SizeOptions>(opt); break;
    case Pert::SEARCH_BAB:
      MinimizeScript::run<Pert,BAB,SizeOptions>(opt); break;
  }

  return 0;

}


