/*

  Global constraint circuit using "orbits" in Gecode.

  The following model implements the constraint circuit using permutation 
  orbits of the first element in the array (x). (It was created when playing 
  around with some other problem in MiniZinc.)

  For more about the circuit constraint, see the 
  Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html

  Comparing with the simple model using the Gecode's builtin circuit

    // ....
    circuit(*this, x, n-1, opt.icl());
    branch(*this, x, INT_VAR_SIZE_MIN, INT_VAL_MIN);
    // ...

  the two models has the same statistics:
    - 0 failures
    - peak depth
    - nodes
  But not the same number of propagations, which is probably not a surprise.
  Via occular investigation Gist it seems that they have the same search tree.
  However, the order of solutions are not identical in the two models (given
  the same branchings).
  

  Compare with the MiniZinc model: 
  * MiniZinc: http://www.hakank.org/minizinc/circuit_test.mzn
    Note: The MiniZinc model is 1-based, this Gecode model is 0-based.


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

void circuit_orbit(Space &space, IntVarArray x, IntConLevel icl = ICL_BND) {

  int n = x.size();

  /**
   * z is an temporary array which contains the orbits of x[0].
   *
   * Basic condition: z[i] must not be 0 until i = n-1 and 
   * then it must be 0
   */
  // IntVarArray z(*this, n, 0, n-1);
  IntVarArray z(space, n, 0, n-1);

  distinct(space, x, icl);
  distinct(space, z, icl);
  
  // put the orbit of x[0] in in z[1..n]
  rel(space, z[0] == x[0], icl);
  
  for(int i = 1; i < n; i++) {
    // z[i] = x[z[i-1]]
    // element(space, x, z[i-1], z[i], icl);
    rel(space, z[i] == element(x, z[i-1]));
  }
  
  // may not be 0 for i < n-1
  for(int i = 0; i < n-1; i++) {
    rel(space, z[i] != 0, icl);
  }
  
  // when i = n-1 it must be 0
    rel(space, z[n-1] == 0, icl);
    
}

class CircuitOrbit : public Script {
protected:

  const int n;   // size of problem
  IntVarArray x; // the array

public:

  CircuitOrbit(const SizeOptions& opt) 
    : 
    n(opt.size()),
    x(*this, n, 0, n-1)
  {

    circuit_orbit(*this, x, opt.icl());

    branch(*this, x, INT_VAR_DEGREE_MIN(), INT_VAL_SPLIT_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << x << std::endl;

  }


  // Constructor for cloning s
  CircuitOrbit(bool share, CircuitOrbit& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CircuitOrbit(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("CircuitOrbit");

  opt.solutions(1);
  opt.icl(ICL_VAL);

  opt.parse(argc,argv);
  if (!opt.size()) {
    opt.size(100);
  }

  opt.c_d(opt.size()*2);

  Script::run<CircuitOrbit,DFS,SizeOptions>(opt);

  return 0;
}
