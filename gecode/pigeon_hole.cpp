/*

  Pigeon hole problem in Gecode.

  ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
  """
  pigeon: the pigeon-hole problem consists in putting n pigeons in m 
  pigeon-holes (at most 1 pigeon per hole). The boolean formulation uses 
  n - m variables to indicate, for each pigeon, its hole number. 
  Obviously, there is a solution iff n <= m.
  """

  Compare with the following models:
  * Comet: http://www.hakank.org/comet/pigeon_hole.co
  * MiniZinc: http://www.hakank.org/minizinc/pigeon_hole.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/pigeon_hole.pl
  * ECLiPSe: http://www.hakank.org/eclipse/pigeon_hole.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class PigeonHole : public Script {
protected:

  static const int N = 3; // N pigeons
  static const int M = 10; // N pigeons
  IntVarArray pigeon_holes;

public:

  PigeonHole(const Options& opt) 
  : 
    pigeon_holes(*this, M*N, 0, 1)
  {
    
    Matrix<IntVarArray> m(pigeon_holes, M, N);

    //  max 1 pigeon per pigeon hole
    /*
   forall(j in 1..M) (
     sum([p[i,j] | i in 1..N]) <= 1
   )
    */
    for(int j = 0; j < M; j++) {
      rel(*this, sum(m.col(j)) <= 1);
    }

   // all pigeon must be placed and only at one hole
    /*
   forall(i in 1..N) (
     sum([p[i,j] | j in 1..M]) = 1
   )
    */
    for(int i = 0; i < N; i++) {
      rel(*this, sum(m.row(i)) == 1);
    }

    branch(*this, pigeon_holes, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    // os << "pigeon_holes: " << pigeon_holes << endl;
    Matrix<IntVarArray> m(pigeon_holes, M, N);
    os << m << endl;
    os << endl;
  }

  // Constructor for cloning s
  PigeonHole(bool share, PigeonHole& s) : Script(share,s) {
    pigeon_holes.update(*this, share, s.pigeon_holes);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new PigeonHole(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("PigeonHole");
  opt.parse(argc,argv);

  opt.solutions(0);
  opt.parse(argc,argv);
  Script::run<PigeonHole,DFS,Options>(opt);

  return 0;
}


