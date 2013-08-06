/*

  Place number puzzle in Gecode.

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  """
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/place_number.mzn
  * Comet: http://www.hakank.org/comet/place_number_puzzle.co
  * ECLiPSe: http://www.hakank.org/eclipse/place_number_puzzle.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/place_number_puzzle.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;

class PlaceNumber : public Script {
protected:

  static const int n = 8;
  IntVarArray x;

public:

  PlaceNumber(const SizeOptions& opt) 
    : 
    x(*this, n, 1, n) 
  {

    int m = 32;
    int _graph[] = 
      {
        1,2,
        1,3,
        1,4,
        2,1,
        2,3,
        2,5,
        2,6,
        3,2,
        3,4,
        3,6,
        3,7,
        4,1,
        4,3,
        4,6,
        4,7,
        5,2,
        5,3,
        5,6,
        5,8,
        6,2,
        6,3,
        6,4,
        6,5,
        6,7,
        6,8,
        7,3,
        7,4,
        7,6,
        7,8,
        8,5,
        8,6,
        8,7
      };
    IntArgs graph(2*m, _graph);

    
    distinct(*this, x);

    for(int i = 0; i < m; i++) {
      int g1 = graph[i*2+0]-1;
      int g2 = graph[i*2+1]-1;
      rel(*this, abs(x[g1]-x[g2]) > 1);
    }
  
    // symmetry breaking
    rel(*this, x[0] < x[n-1]);

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    os << x << endl;
  }

  // Constructor for cloning s
  PlaceNumber(bool share, PlaceNumber& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new PlaceNumber(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("PlaceNumber");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<PlaceNumber,DFS,SizeOptions>(opt);

  return 0;
}


