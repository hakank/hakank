/*

  Combinatorial auction problem in Gecode.


  http://en.wikipedia.org/wiki/Combinatorial_auction
  """
  A combinatorial auction is an auction in which bidders can place 
  bids on combinations of items, or "packages," rather than 
  just individual items. Simple combinatorial auctions have been 
  used for many years in estate auctions, where a common procedure 
  is to auction the individual items and then at the end to accept 
  bids for packages of items.
  """

  This simple example is from the lecture slides
  Constraint Satisfaction Problems, Constraint Optimization
  by Bernhard Nebel and Stefan WÃ¶lfl
  http://www.informatik.uni-freiburg.de/~ki/teaching/ws0910/csp/csp10-handout4.pdf
  """
  In combinatorial auctions, bidders can give bids for set of items.
  The auctioneer [then] has to generate an optimial selection, e.g.
  one that maximizes revenue.
  
  Definition
  The combinatorial auction problem  is specified as follows:
    Given: A set of items Q = {q1,...,qn} and a set of bids
           B = {b1,...,bm} such that each bid is bi = (Qi, ri),
           where Qi (= Q and ri is a strictly positive real number.
    Task: Find a subset of bids B'(= B such that any two bids in B'
          do not share an item maximizing Sum(Qi,ri) (= Biri.

  ...

  Example Auction

  Consider the following auction:
    b1 = {1,2,3,4}, r1 = 8
    b2 = {2,3,6},   r2 = 6
    b3 = {1,4,5},   r3 = 5
    b4 = {2,8},     r4 = 2
    b5 = {5,6},     r5 = 2

  What is the optimal assignment?
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/combinatorial_auction.mzn

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class CombinatorialAuction : public MaximizeScript {
protected:

  static const int num_bids = 5;
 
  IntVarArray x; 
  IntVar total;

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  }
;

  CombinatorialAuction(const SizeOptions& opt) 
    : 
    x(*this, num_bids, 0, 1),
    total(*this, 0, 1000)
  {

    static const int num_items = 6;

    int _bids[] = {8,6,5,2,2};
    IntArgs bids(num_bids, _bids);

    /* // 1-based
    int packages = {
      4,  1,2,3,4,
      3,  2,3,6,
      3,  1,4,5,
      2,  2,8,
      2,  5,6,
    };
    */

    IntSetArgs packages(num_bids);
    packages[0] = IntSet( IntArgs() << 0 << 1 << 2 << 3);
    packages[1] = IntSet( IntArgs() << 1 << 2 << 5);
    packages[2] = IntSet( IntArgs() << 0 << 3 << 4);
    packages[3] = IntSet( IntArgs() << 1 << 7);
    packages[4] = IntSet( IntArgs() << 4 << 5);

    rel(*this, sum(bids, x) == total);

    // ensure that each items is selected exactly once
    for(int j = 0; j < num_items; j++) {
      BoolVarArgs tmp;
      for(int i = 0; i < num_bids; i++) {
        if (packages[i].in(j)) {
          tmp << expr(*this, x[i] == 1);
        }
      }
      rel(*this, sum(tmp) == 1);
    }

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "total: " << total << endl;
    os << "x: " << x << endl;
    os << endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return total;
  }


  // Constructor for cloning s
  CombinatorialAuction(bool share, CombinatorialAuction& s) : MaximizeScript(share,s) {
    x.update(*this, share, s.x);
    total.update(*this, share, s.total);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CombinatorialAuction(share,*this);
  }

};


int
main(int argc, char* argv[]) {
  SizeOptions opt("CombinatorialAuction");
  opt.solutions(0);
  opt.iterations(20000);

  opt.search(CombinatorialAuction::SEARCH_BAB);
  opt.search(CombinatorialAuction::SEARCH_DFS, "dfs");
  opt.search(CombinatorialAuction::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (opt.search() == CombinatorialAuction::SEARCH_DFS) {
    Script::run<CombinatorialAuction,DFS,SizeOptions>(opt);
  } else {
    MaximizeScript::run<CombinatorialAuction,BAB,SizeOptions>(opt);
  }


  return 0;
}


