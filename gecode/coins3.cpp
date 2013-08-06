/*

  Coin application in Gecode.

 
  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/coins3.mzn
  * Comet: http://www.hakank.org/comet/coins3.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class Coins3 : public MinimizeScript {
protected:

  static const int n = 6; // number of different coins

  int num_coins_val;      // set the number of coins (for showing all solutions)

  IntVarArray x;          // array for number of each coins
  IntVar num_coins;       // number of coins used (to minimize)

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  Coins3(const SizeOptions& opt) 
  : 
    num_coins_val(opt.size()),
    x(*this, n, 0, 99),
    num_coins(*this, 0, 99)
  {

    // values of the coins
    int _variables[] = {1, 2, 5, 10, 25, 50}; 
    IntArgs variables(n, _variables); 

    // sum the number of coins
    linear(*this, x, IRT_EQ, num_coins, opt.icl());

    // This is the "main loop":
    // Checks that all changes from 1 to 99 can be made
    for(int j = 0; j < 99; j++) {
      IntVarArray tmp(*this, n, 0, 99);
      linear(*this, variables, tmp, IRT_EQ, j, opt.icl());
      for(int i = 0; i < n; i++) {
        rel(*this, tmp[i] <= x[i], opt.icl());
      }
    }

    // set the number of coins (via opt.size())
    // don't forget 
    //  -search dfs
    if (num_coins_val) {
      rel(*this, num_coins == num_coins_val, opt.icl());
    }

    branch(*this, x, INT_VAR_SIZE_MAX(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "num_coins: " << num_coins << std::endl;
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Coins3(bool share, Coins3& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    num_coins.update(*this, share, s.num_coins);
  }

  // Return cost
  virtual IntVar cost(void) const {
    return num_coins;
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Coins3(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Coins3");
  opt.solutions(0);

  opt.search(Coins3::SEARCH_BAB);
  opt.search(Coins3::SEARCH_DFS, "dfs");
  opt.search(Coins3::SEARCH_BAB, "bab");

  opt.parse(argc,argv);
  switch (opt.search()) {
    case Coins3::SEARCH_DFS:
      MinimizeScript::run<Coins3,DFS,SizeOptions>(opt); break;
    case Coins3::SEARCH_BAB:
      MinimizeScript::run<Coins3,BAB,SizeOptions>(opt); break;
    }

  return 0;

}


