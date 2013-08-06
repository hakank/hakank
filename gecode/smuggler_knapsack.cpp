/*
  
  Smuggler's knapsack problem in Gecode.

  Marriott & Stuckey: 'Programming with constraints', page  101f, 115f

  Smuggler's knapsack.
  
  A smuggler has a knapsack with a capacity of 9 units.
              Unit       Profit
  Whisky:     4 units    15 dollars
  Perfume:    3 units    10 dollars
  Cigarettes: 2 units     7 dollars

  What is the optimal choice?

  Compare with the following models:
  * ECLiPSe: http://www.hakank.org/eclipse/smuggler_knapsack.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/smuggler_knapsack.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class SmugglersKnapsack : public MaximizeScript {
protected:

  static const int n = 3;
  static const int c = 9;

  IntVarArray x;
  IntVar TotalProfit;

public:

  SmugglersKnapsack(const Options& opt) 
    : 
    x(*this, n, 0, c),
    TotalProfit(*this, 0, 10000)
  {

    int _units[] = {4,3,2};
    IntArgs units(n, _units);

    int _profits[] = {15,10,7};
    IntArgs profits(n, _profits);
    

    IntVar
      W(x[0]), 
      P(x[1]), 
      C(x[2]);

    // "talkative" version
    /*
    rel(*this,
        // Units
        4*W  + 3*P  + 2*C <= c &&
        // Profit.
        15*W + 10*P + 7*C == TotalProfit
        );
    */
    
    // More general version
    linear(*this, units, x, IRT_LQ, c);
    linear(*this, profits, x, IRT_EQ, TotalProfit);

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MAX());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "TotalProfit: " << TotalProfit << endl;
    os << "x: " << x << endl;
    os << endl;

  }


  // Constructor for cloning s
  SmugglersKnapsack(bool share, SmugglersKnapsack& s) : MaximizeScript(share,s) {
    x.update(*this, share, s.x);
    TotalProfit.update(*this, share, s.TotalProfit);
  }

  virtual IntVar cost(void) const {
    return TotalProfit;
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SmugglersKnapsack(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("SmugglersKnapsack");

  opt.solutions(0);

  opt.parse(argc,argv);

  MaximizeScript::run<SmugglersKnapsack,BAB,Options>(opt);
    
  return 0;
}


