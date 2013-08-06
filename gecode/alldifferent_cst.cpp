/*

  Global constraint alldifferent_cst in Gecode.

  This model implements a decomposition of the global constraint
  alldifferent_cst.

  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_cst.html
  """
  For all pairs of items‹(VARIABLES[i], VARIABLES[j]) (i!=j) of the 
  collection VARIABLES enforce 
  VARIABLES‹[i].var+VARIABLES‹[i].cst != VARIABLES‹[j].var+VARIABLES[j].cst.
  
  Example
   (<
      var5 cst0,
      var1 cst1,
      var9 cst0,
      var3 cst4
   >
   )
  
  The alldifferent_cst constraint holds since all the expressions 
  5+0=5, 1+1=2, 9+0=9 and 3+4=7 correspond to distinct values.
  """


  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/alldifferent_cst.mzn
  - SICStus Prolog: http://www.hakank.org/sicstus/alldifferent_cst.pl
  - ECLiPSe: http://www.hakank.org/eclipse/alldifferent_cst.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void alldifferent_cst(Space& space, IntVarArray x, IntVarArray cst, int n, IntConLevel icl = ICL_BND) {

  IntVarArgs adds;
  for(int i = 0; i < n; i++) {
    adds << expr(space, x[i] + cst[i]);
  }
  
  distinct(space, adds);

}

class AlldifferentCst : public Script {
protected:

  static const int n = 4;

  IntVarArray x;
  IntVarArray cst;

public:

  // Actual model
  AlldifferentCst(const SizeOptions& opt) : 
    x(*this, n, 1, 9),
    cst(*this, n, 0, 9)
  {

    int _cst1[] = {0,1,0,4};
    IntArgs cst1(n, _cst1);
    for(int i = 0; i < n; i++) {
      rel(*this, cst[i] == cst1[i]);
    }

    alldifferent_cst(*this, x, cst, n, opt.icl());

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  AlldifferentCst(bool share, AlldifferentCst& s) : Script(share,s) {
    x.update(*this, share, s.x);
    cst.update(*this, share, s.cst);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AlldifferentCst(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "cst: " << cst << std::endl;
    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("AlldifferentCst");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<AlldifferentCst,DFS,SizeOptions>(opt);

  return 0;
}


