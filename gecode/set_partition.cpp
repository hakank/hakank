/*

  Set partition problem in Gecode.

  Problem formulation from
    http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
   This is a partition problem.
   Given the set S = {1, 2, ..., n}, 
   it consists in finding two sets A and B such that:
   <ul>
   <li>A U B = S,</li>
   <li>|A| = |B|,</li>
   <li>sum(A) = sum(B),</li>
   <li>sum_squares(A) = sum_squares(B).</li>
   </ul>
  """

  Compare with my other models:
  * MiniZinc: http://www.hakank.org/minizinc/set_partition.mzn
  * Gecode/R: http://www.hakank.org/gecode_r/set_partition.rb
  * Comet   : http://www.hakank.org/comet/set_partition.co
  * ECLiPSe : http://www.hakank.org/eclipse/set_partition.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/set_partition.pl

  Note: In the Gecode distribution there is a model that solves
  the same problem but a different approach. 
  This current model uses set variables, whereas the distributed
  model uses integer variables (and a lot of efficiency constraints).


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class SetPartition : public Script {
protected:

  static const int num_sets = 2;

  int n;

  SetVarArray a;
  IntVarArray sums;
  IntVarArray sum_squared;

public:

  SetPartition(const SizeOptions& opt) 
  : 
    n(opt.size()),
    a(*this, num_sets, IntSet::empty, IntSet(1, n)),
    sums(*this, num_sets, 0,n*n),
    sum_squared(*this, num_sets, 0,n*n*n)
  {

    // cardinality of each set
    int card = n / num_sets;

    // create sets for the weighted sums
    IntArgs weights1;
    IntArgs weights_squared;
    for(int i = 1; i <= n; i++) {
      weights1 << i;
      weights_squared<< i*i;
    }

    //  use all the elements in S and it should be disjoint sets
    for(int i = 0; i < num_sets; i++) {
      cardinality(*this, a[i], card, card);
    }
    // disjoint sets
    for(int i = 0; i < num_sets; i++) {
      for(int j = 0; j < i; j++) {
        rel(*this, a[i] || a[j]);
      }
    }

    // get the sums
    for(int i = 0; i < num_sets; i++) {
      weights(*this, weights1, weights1, a[i], sums[i]);
      weights(*this, weights1, weights_squared, a[i], sum_squared[i]);
    }

    // the sets have equal sums and equal squared sums
    for(int i = 1; i < num_sets; i++) {
      rel(*this, sums[i] == sums[i-1]);
      rel(*this, sum_squared[i] == sum_squared[i-1]);
    }

    // symmetry breaking: 1 is in the first set
    rel(*this, a[0], SRT_SUP, IntVar(*this,1,1));

    branch(*this, a, SET_VAR_SIZE_MAX(), SET_VAL_MAX_INC());
    /*
    branch(*this, sums, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, sum_squared, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    */

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "a          : " << a << std::endl;
    os << "sums       : " << sums << std::endl;
    os << "sum_squared: " << sum_squared << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  SetPartition(bool share, SetPartition& s) : Script(share,s), n(s.n)  {
    a.update(*this, share, s.a);
    sums.update(*this, share, s.sums);
    sum_squared.update(*this, share, s.sum_squared);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SetPartition(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("SetPartition");

  opt.solutions(0);
  opt.size(12);
  opt.parse(argc,argv);

  /*
  if (opt.size() % 2 == 1) {
    std::cerr << "Size " << opt.size() << " is not even." << endl;
    return 1;
  }
  */

  Script::run<SetPartition,DFS,SizeOptions>(opt);

  return 0;
}


