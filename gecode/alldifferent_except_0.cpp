/*

  Global constraint alldifferent_except_0 in Gecode.

  Decomposition version of the global constraint alldifferent_except_0.

  From Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
  """
  Enforce all variables of the collection VARIABLES to take distinct values, 
  except those variables that are assigned to 0.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/alldifferent_except_0.mzn
  * Comet   : http://www.hakank.org/alldifferent_except_0.co

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


/**
 *
 * Decomposition variant of the global constraint alldifferent_except_0
 *
 * alldifferent_except_0(space, x, icl)
 *
 *  space : Space
 *  x: IntVarArray
 *  icl: IntConLevel, default ICL_BND
 *
 */
void alldifferent_except_0(Space& space, IntVarArray x, IntConLevel icl = ICL_BND) {

  for(int i = 0; i < x.size(); i++) {
    for(int j = i+1; j < x.size(); j++) {
      rel(space,
          (x[i] != 0 && x[j] != 0) >> (x[i] != x[j]),
          icl
          );

    }
  }
  
} // alldifferent_except_0


/**
 *
 * Decomposition variant of alldifferent_except c,
 * (a somewhat more general than alldifferent_except_0)
 *
 * alldifferent_except_c(space, x, c, icl)
 *
 *  space : Space
 *  x: IntVarArray
 *  c: int, the value which should be excluded
 *  icl: IntConLevel, default ICL_BND
 *
 */
void alldifferent_except_c(Space& space, IntVarArray x, int c, IntConLevel icl = ICL_BND) {

  for(int i = 0; i < x.size(); i++) {
    for(int j = i+1; j < x.size(); j++) {
      rel(space,
           (x[i] != c && x[j] != c) >> (x[i] != x[j]),
           icl
           );
    }
  }
  
} // alldifferent_except_c




class AlldifferentExcept0 : public Script {
protected:

  int size;       // length of array, from opt.size()
  IntVarArray x;  // array of elements
  IntVar z;       // number of 0 in x

public:

  AlldifferentExcept0(const SizeOptions& opt) 
  : 
    size(opt.size()),
    x(*this, size, 0, size-1),
    z(*this, 0, size)
  {

    
    // alldifferent_except_0(*this, x, opt.icl());
    alldifferent_except_c(*this, x, 0, opt.icl()); // general version

    // extra constraint

    // count the number of 0's
    count(*this, x, 0, IRT_EQ, z, opt.icl()); 

    // there should be at least one 0    
    rel(*this, z, IRT_GR, 0, opt.icl()); 

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << " z: " << z << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  AlldifferentExcept0(bool share, AlldifferentExcept0& s) : Script(share,s) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AlldifferentExcept0(share,*this);
  }

};


int
main(int argc, char* argv[]) {

    SizeOptions opt("AlldifferentExcept0");
    opt.icl(ICL_DOM);
    opt.solutions(0);

    opt.parse(argc,argv);
    if (!opt.size()) 
      opt.size(3);
    
    Script::run<AlldifferentExcept0,DFS,SizeOptions>(opt);

    return 0;
}


