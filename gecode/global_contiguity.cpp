/*

  Global constraint global contiguity (decompositions) in Gecode.

  From Global constraint catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  """
  Enforce all variables of the VARIABLES collection to be assigned to 
  0 or 1. In addition, all variables assigned to value 1 appear contiguously.
  """

  For more about this constraint, see
  Michael J. Maher: "Analysis of a Global Contiguity Constraint"
  http://www.cse.unsw.edu.au/~mmaher/pubs/cp/contig.pdf 


  This model implements two variants:
  1) An implementation inspired by Toby Walsh's presentation 
    "Sliding Constraints"
     http://www.cse.unsw.edu.au/~tw/samos/slide.ppt
     where he defines global cardinality in terms of the global 
     constraint slide.

  2) Simply use the regular expression "0*1*0*"


  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/global_contiguity.mzn
  * Comet   : http://www.hakank.org/comet/global_contiguity.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class GlobalContiguity : public Script {
protected:

  const int n;   // size of problem
  IntVarArray x; // the array

public:

  enum {
    MODEL_WALSH,
    MODEL_REGEXP
  };

  GlobalContiguity(const SizeOptions& opt) 
    : 
    n(opt.size()),
    x(*this, n, 0, 1)
  {

    if (opt.model() == MODEL_WALSH) {

      /**
       *
       * Model inspired by Toby Walsh's presentation
       * "Sliding Constraints"
       * http://www.cse.unsw.edu.au/~tw/samos/slide.ppt
       *
       */
      IntVarArray y(*this, n, 0, 2);
      rel(*this, y, IRT_GQ, opt.icl());
      for(int i = 0; i < n; i++) {
        rel(*this, (y[i] == 1) == (x[i] == 1), 
             opt.icl());
      }

    } else {

      /**
       *
       * Using the regular expression 0*1*0*
       *
       */
      REG r0(0), r1(1);
      extensional(*this, x, *r0 + *r1 + *r0);
    }


    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << x << std::endl;

  }


  // Constructor for cloning s
  GlobalContiguity(bool share, GlobalContiguity& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new GlobalContiguity(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("GlobalContiguity");

  opt.solutions(1);
  opt.icl(ICL_VAL);
  opt.model(GlobalContiguity::MODEL_WALSH);
  opt.model(GlobalContiguity::MODEL_WALSH, "walsh");
  opt.model(GlobalContiguity::MODEL_REGEXP, "regexp");

  opt.parse(argc,argv);
  if (!opt.size()) {
    opt.size(100);
  }

  opt.c_d(opt.size()*2);

  Script::run<GlobalContiguity,DFS,SizeOptions>(opt);

  return 0;
}
