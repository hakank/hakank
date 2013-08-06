/*

  Safe cracking problem in Gecode.

  From the Oz Primer:
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The code of Professor Smart's safe is a sequence of 9 distinct 
  nonzero digits C1 .. C9 such that the following equations and
  inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

   and

 
   C1 <> 1, C2 <> 2, ..., C9 <> 9

   can you find the correct combination?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/safe_cracking.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/safe_cracking.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/safe_cracking.pl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;

class SafeCracking : public Script {
protected:

  static const int n = 9; 

  IntVarArray x;

public:

  SafeCracking(const SizeOptions& opt) 
    : 
    x(*this, n, 1, n)
  {

    IntVar
      C1(x[0]),
      C2(x[1]),
      C3(x[2]),
      C4(x[3]),
      C5(x[4]),
      C6(x[5]),
      C7(x[6]),
      C8(x[7]),
      C9(x[8]);

    distinct(*this, x);

    for(int i = 0; i < n; i++) {
      rel(*this, x[i] != i+1);
    }

    rel(*this,
        C4 - C6 == C7 &&
        C1 * C2 * C3 == C8 + C9 &&
        C2 + C3 + C6 < C8 &&
        C9 < C8
        );


    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;
  }

  // Constructor for cloning s
  SafeCracking(bool share, SafeCracking& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SafeCracking(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("SafeCracking");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<SafeCracking,DFS,SizeOptions>(opt);

  return 0;
}


