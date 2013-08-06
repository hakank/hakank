/*

  Global constraint alldifferent_modulo in Gecode.

  This model implements a decomposition of the global constraint
  alldifferent_modulo.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_modulo.html
  """
  Enforce all variables of the collection VARIABLES to have a distinct 
  rest when divided by M.
  
  Example
  (<25, 1, 14, 3>, 5)
  
  The equivalence classes associated with values 25, 1, 14 and 3 are 
  respectively equal to 
     25â mod 5 = 0, 1 mod 5 = 1, 14 mod 5 = 4 and 3 mod 5 = 3. 
  Since they are distinct the alldifferent_modulo constraint holds.
  """


  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/alldifferent_modulo.mzn
  - SICStus Prolog: http://www.hakank.org/sicstus/alldifferent_modulo.pl
  - ECLiPSe: http://www.hakank.org/eclipse/alldifferent_modulo.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void alldifferent_modulo(Space& space, IntVarArray x, int n, IntVar m, IntConLevel icl = ICL_BND) {

  IntVarArgs mods;
  for(int i = 0; i < n; i++) {
    mods << expr(space, x[i] % m);
  }
  
  distinct(space, mods);

}

class AlldifferentModulo : public Script {
protected:

  static const int n = 4;

  IntVarArray x;
  IntVar m;

public:

  // Actual model
  AlldifferentModulo(const SizeOptions& opt) : 
    x(*this, n, 1, 25),
    m(*this, 1, 5)
  {

    int _tmp[] = {25,1,14,3};
    IntArgs tmp(n, _tmp);
    for(int i = 0; i < n; i++) {
      rel(*this, x[i] == tmp[i]);
    }

    alldifferent_modulo(*this, x, n, m, opt.icl());
    // rel(*this, m==5);

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, m, INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  AlldifferentModulo(bool share, AlldifferentModulo& s) : Script(share,s) {
    x.update(*this, share, s.x);
    m.update(*this, share, s.m);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AlldifferentModulo(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "m: " << m << std::endl;
    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("AlldifferentModulo");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<AlldifferentModulo,DFS,SizeOptions>(opt);

  return 0;
}


