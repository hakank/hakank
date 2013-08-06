/*

  Global constraint all_differ_from_at_least_k_pos in Gecode.

  This model implements a decomposition of the global constraint
  all_differ_from_at_least_k_pos.

  Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_different_from_at_least_k_pos.html
  """
  Enforce all pairs of distinct vectors of the VECTORS collection to differ 
  from at least K positions.
  
  Example
  (
   2, <
   vec-<2, 5, 2, 0>,
   vec-<3, 6, 2, 1>,
   vec-<3, 6, 1, 0>
   >
  )
  
  The all_differ_from_at_least_k_pos constraint holds since:
   * The first and second vectors differ from 3 positions, which is 
     greater than or equal to K=2.
   * The first and third vectors differ from 3 positions, which is greater 
     than or equal to K=2.
   * The second and third vectors differ from 2 positions, which is greater 
     than or equal to K=2.
  """
  
  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/all_differ_from_at_least_k_pos.mzn
  - SICStus Prolog: http://www.hakank.org/sicstus/all_differ_from_at_least_k_pos.pl
  - ECLiPSe: http://www.hakank.org/eclipse/all_differ_from_at_least_k_pos.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void all_differ_from_at_least_k_pos(Space& space, IntVarArray x, IntVar k_pos, int rows, int cols, IntConLevel icl = ICL_BND) {

    for(int i = 0; i < rows; i++) {
      for(int j = 0; j < rows; j++) {
        if (i != j) {
          BoolVarArgs tmp;
          for(int k = 0; k < cols; k++) {
            tmp << expr(space, x[i*cols+k] != x[j*cols+k]);
          }
          rel(space, sum(tmp) >= k_pos);
        }
      }
    }

}

class AllDifferFromAtLeastKPos : public Script {
protected:

  static const int rows = 3;
  static const int cols = 4;

  IntVarArray x;
  IntVar k_pos;

public:

  // Actual model
  AllDifferFromAtLeastKPos(const SizeOptions& opt) : 
    x(*this, rows*cols, 0, 6),
    k_pos(*this, 0, cols)
  {

    // fill the data
    int _g[] = {
      2,5,2,0,
      3,6,2,1,
      3,6,1,0
    };
    IntArgs g(rows*cols, _g);
    for(int i = 0; i < rows*cols; i++) {
      rel(*this, x[i] == g[i]);
    }


    all_differ_from_at_least_k_pos(*this, x, k_pos, rows, cols, opt.icl());

    rel(*this, k_pos == 2);
      
    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    // branch(*this, k_pos, INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  AllDifferFromAtLeastKPos(bool share, AllDifferFromAtLeastKPos& s) : Script(share,s) {
    x.update(*this, share, s.x);
    k_pos.update(*this, share, s.k_pos);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AllDifferFromAtLeastKPos(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "k_pos: " << k_pos << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("AllDifferFromAtLeastKPos");
  opt.solutions(0);
  opt.parse(argc,argv);

  if (!opt.size()) 
    opt.size(4);

  Script::run<AllDifferFromAtLeastKPos,DFS,SizeOptions>(opt);

  return 0;
}


