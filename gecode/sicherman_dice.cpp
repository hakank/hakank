/*

  Sicherman Dice in Gecode.

  From http://en.wikipedia.org/wiki/Sicherman_dice
  """ 
  Sicherman dice are the only pair of 6-sided dice which are not normal dice, 
  bear only positive integers, and have the same probability distribution for 
  the sum as normal dice.
  
  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  """

  I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/sicherman_dice.mzn

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class SichermanDice : public Script {
protected:

  const static int n = 6;
  const static int m = 10; // max integer 

  IntVarArray x1; // first
  IntVarArray x2; // second

public:


  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
  };


  SichermanDice(const SizeOptions& opt) 
  : 
    x1(*this, n, 1, m),
    x2(*this, n, 1, m)
  {

    // the standard distribution of two dice
    int standard_dist[] = {1,2,3,4,5,6,5,4,3,2,1};

    // get the distribution
    for(int k = 2; k <= 12; k++) {
      BoolVarArgs tmp;
      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          tmp << expr(*this, x1[i] + x2[j] == k);
        }
      }
      rel(*this, standard_dist[k-2] == sum(tmp));
    }

    // symmetry breaking
    rel(*this, x1, IRT_LQ);
    rel(*this, x2, IRT_LQ);

    // x1 is lexicographic less or equal to x2
    rel(*this, x1, IRT_LQ, x2);

    branch(*this, x1, INT_VAR_NONE(), INT_VAL_MIN());  
    branch(*this, x2, INT_VAR_NONE(), INT_VAL_MIN());  

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x1: " << x1 << endl;
    os << "x2: " << x2 << endl;
    os << endl;
  }

  // Constructor for cloning s
  SichermanDice(bool share, SichermanDice& s) : Script(share,s) {
    x1.update(*this, share, s.x1);
    x2.update(*this, share, s.x2);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SichermanDice(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("SichermanDice");

  opt.icl(ICL_DEF);
  opt.search(SichermanDice::SEARCH_DFS);
  opt.search(SichermanDice::SEARCH_DFS, "dfs");

  opt.solutions(0);
  opt.parse(argc,argv);
  
  switch (opt.search()) {
    case SichermanDice::SEARCH_DFS:MinimizeScript::run<SichermanDice,DFS,SizeOptions>(opt); break;
  }
  
  
  return 0;
}


