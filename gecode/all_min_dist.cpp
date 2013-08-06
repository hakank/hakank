/*

  Global constraint all_min_dist in Gecode.

  This model implements a decomposition of the global constraint
  all_min_dist.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_min_dist.html
  """
  Enforce for each pair (vari, varj)â€‹ of distinct variables of the 
  collection VARIABLES that 
  |vari - varj| >= MINDIST.
  
  Example
   (2, <5, 1, 9, 3>)
  
  The all_min_dist constraint holds since the following expressions 
  |5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
  to the first argument MINDIST = 2 of the all_min_dist constraint.
  """

  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/all_min_dist.mzn
  - SICStus Prolog: http://www.hakank.org/sicstus/all_min_dist.pl
  - ECLiPSe: http://www.hakank.org/eclipse/all_min_dist.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void all_min_dist(Space& space, IntVar c, IntVarArray x, int n, IntConLevel icl = ICL_BND) {

    for(int i = 0; i < n; i++) {
      for(int j = 0; j < i; j++) {
        rel(space, abs(x[i] - x[j]) >= c);
      }
    }
}

class AllMinDist : public Script {
protected:

  static const int n = 4;

  IntVarArray x;
  IntVar c;

public:

  // Actual model
  AllMinDist(const SizeOptions& opt) : 
    x(*this, n, 1, 9),
    c(*this, 0, 9)
  {

    int _t[] = {5,1,9,3};
    IntArgs t(n, _t);
    for(int i = 0; i < n; i++) {
      rel(*this, x[i] == t[i]);
    }

    all_min_dist(*this, c, x, n, opt.icl());
    rel(*this, c == 2);

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  AllMinDist(bool share, AllMinDist& s) : Script(share,s) {
    x.update(*this, share, s.x);
    c.update(*this, share, s.c);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AllMinDist(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "c: " << c << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("AllMinDist");
  opt.solutions(0);
  opt.parse(argc,argv);

  if (!opt.size()) 
    opt.size(4);

  Script::run<AllMinDist,DFS,SizeOptions>(opt);

  return 0;
}


