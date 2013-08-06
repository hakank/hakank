/*

  Subset sum problem in Gecode.


  From Katta G. Murty: "Optimization Models for Decision Making", page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  """
  Example 7.8.1
  
  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/subset_sum.mzn
  * Comet   : http://www.hakank.org/comet/subset_sum.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


/**
 * subset_sum(values, x, tot) 
 *  where 
 *    values is the values to choose from (the coin values)
 *    x contains the number from each bag
 *    total is the total value to sum
 *
 */
void subset_sum(Space &space, IntArgs values, IntVarArray x, int tot, IntConLevel icl = ICL_BND) {

  linear(space, values, x, IRT_EQ, tot,  icl);

}



class SubsetSum : public Script {
protected:

  static const int n = 6;  // number of bags
  static const int max_bags = 1000; // some large value

  IntVar num_bags;         // number of of bags stolen
  IntVarArray x;           // array of bags

  const int total;         // parameter from command line: total sum (default 100)

public:


  SubsetSum(const SizeOptions& opt) 
    : 
    num_bags(*this, 0, n*max_bags),
    x(*this, n, 0, max_bags),
    total(opt.size())
  {

    int _coins[]= {16, 17, 23, 24, 39, 40};
    IntArgs coins(n, _coins);
    std::cout << "bag of coins " << coins << std::endl;

    // total number of bags to take
    linear(*this, x, IRT_EQ, num_bags, opt.icl());

    subset_sum(*this, coins, x, total, opt.icl());

    branch(*this, x, INT_VAR_DEGREE_MAX(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {

    os << "number of bags taken: " << num_bags << std::endl;
    os << "x: " << x << std::endl;
    os << std::endl;

  }

  // Constructor for cloning s
  SubsetSum(bool share, SubsetSum& s) : Script(share,s), total(s.total) {
    x.update(*this, share, s.x);
    num_bags.update(*this, share, s.num_bags);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SubsetSum(share,*this);
  }

};


int
main(int argc, char* argv[]) {
  SizeOptions opt("SubsetSum");

  std::cout << "main " << std::endl;
  opt.solutions(0);
  opt.icl(ICL_BND);
  opt.size(100); // total sum

  opt.parse(argc,argv);
  Script::run<SubsetSum,DFS,SizeOptions>(opt);

  return 0;
}


