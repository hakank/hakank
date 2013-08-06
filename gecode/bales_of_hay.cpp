/*

  Bales of hay problem in Gecode.

  From The Math Less Traveled, 
  "The haybaler", http://www.mathlesstraveled.com/?p=582 
  """
  You have five bales of hay.

  For some reason, instead of being weighed individually, they were weighed 
  in all possible combinations of two. The weights of each of these 
  combinations were written down and arranged in numerical order, without 
  keeping track of which weight matched which pair of bales. The weights, 
  in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.

  How much does each bale weigh? Is there a solution? Are there multiple 
  possible solutions? 
  """


  Compare with my other models:
  * MiniZinc: http://www.hakank.org/minizinc/bales_of_hay.mzn

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class BalesOfHay : public Script {
protected:

  static const int n = 5;

  IntVarArray x;

public:

  BalesOfHay(const Options& opt) 
  : 
    x(*this, n, 0, 50)
  {

    int m = 10;
    int weights[] = {80, 82, 83, 84, 85, 86, 87, 88, 90, 91};

    for(int w = 0; w < m; w++) {
      IntVar i(*this, 0, n-1);
      IntVar j(*this, 0, n-1);

      rel(*this, i < j);
      rel(*this, element(x, i) + element(x, j) == weights[w]);

    }

    // increasing
    rel(*this, x, IRT_LQ);

    branch(*this, x, INT_VAR_SIZE_MAX(), INT_VAL_SPLIT_MAX()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  BalesOfHay(bool share, BalesOfHay& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BalesOfHay(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("BalesOfHay");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<BalesOfHay,DFS,Options>(opt);

  return 0;
}


