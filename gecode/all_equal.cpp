/*

  Global constraint all_equal in Gecode.

  This model implements a decomposition of the global constraint
  all_equal.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_equal.html
  """
  Constraint

      all_equal‹(VARIABLES)

  Purpose

      Enforce all variables of the collection VARIABLES to take 
      the same value.

  Example
      ‹(<5, 5, 5, 5>)

  The all_equal constraint holds since all its variables are fixed to value 5.
  """

  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/all_equal_me.mzn
  - SICStus Prolog: http://www.hakank.org/sicstus/all_equal.pl
  - ECLiPSe: http://www.hakank.org/eclipse/all_equal.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void all_equal(Space& space, IntVarArray x, int n, IntConLevel icl = ICL_BND) {

    for(int i = 1; i < n; i++) {
      rel(space, x[i] == x[i-1]);
    }

}

class AllEqual : public Script {
protected:

  static const int n = 4;

  IntVarArray x;

public:

  // Actual model
  AllEqual(const SizeOptions& opt) : 
    x(*this, n, 0, 6)
  {

    all_equal(*this, x, n, opt.icl());
      
    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  AllEqual(bool share, AllEqual& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AllEqual(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("AllEqual");
  opt.solutions(0);
  opt.parse(argc,argv);

  if (!opt.size()) 
    opt.size(4);

  Script::run<AllEqual,DFS,SizeOptions>(opt);

  return 0;
}


