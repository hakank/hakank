/*

  xkcd knapsack problem in Gecode.

  From http://xkcd.com/287/
 
  Some amount (or none) of each dish should be ordered to give a total 
  of exact 15.05

  See my other models:
  MiniZinc: http://www.hakank.org/minizinc/xkcd.mzn
  Comet: http://www.hakank.org/comet/xkcd.co
  Gecode/R: http://www.hakank.org/gecode_r/xkcd.rb


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class Xkcd : public Script {
protected:

  static const int num_prices = 6; // number of total dishes
  IntVarArray x;                   // the dishes
  IntVar num_dishes;               // number of dishes ordered
public:

  Xkcd(const Options& opt) 
  : 
    x(*this, num_prices, 0, 1000), 
    num_dishes(*this, 0,100) {

    int _price[] = {215, 275, 335, 355, 420, 580};
    int total = 1505; // original problem
    IntArgs price(num_prices);
    for(int i = 0; i < num_prices; i++)
      price[i] = _price[i];

    linear(*this, price, x, IRT_EQ, total, opt.icl());
    linear(*this, x, IRT_EQ, num_dishes, opt.icl());

    // branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());
    // This gives a neater search tree:
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 

  }
  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << x << ": " << num_dishes << " dishes" << std::endl;
  }

  // Constructor for cloning s
  Xkcd(bool share, Xkcd& s) : Script(share,s) {
    x.update(*this, share, s.x);
    num_dishes.update(*this, share, s.num_dishes);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Xkcd(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("Xkcd");
  opt.solutions(0);
  opt.iterations(20000);
  opt.parse(argc,argv);
  Script::run<Xkcd,DFS,Options>(opt);
  return 0;
}


