/*

  Global constraint bin_packing in Gecode.
 
  Decomposition of the global constraint bin_packing.

  From Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cbin_packing.html
  """
  Given several items of the collection ITEMS (each of them having a specific 
  weight), and different bins of a fixed capacity, assign each item to a bin 
  so that the total weight of the items in each bin does not exceed CAPACITY.
  
  Example
    <(5,<bin-3 weight-4, bin-1 weight-3,bin-3 weight-1>)>
  
   The bin_packing constraint holds since the sum of the height of items 
  that are assigned to bins 1 and 3 is respectively equal to 3 and 5. 
  The previous quantities are both less than or equal to the maximum 
  CAPACITY 5. Figure 4.35.1 shows the solution associated with the example.
  
  Remark
  
  Note the difference with the classical bin-packing problem [MT90] where 
  one wants to find solutions that minimise the number of bins. In our 
  case each item may be assigned only to specific bins (i.e., the different 
  values of the bin variable) and the goal is to find a feasible solution. 
  This constraint can be seen as a special case of the cumulative 
  constraint [AB93], where all task durations are equal to 1.
  """

  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/bin_packing2.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/bin_packing2.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/bin_packing2.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class BinPacking : public Script {
protected:

  
  static const int n = 3;

  IntVarArray bins;   
  IntVarArray weights;   

  IntVar capacity;

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
  };

  BinPacking(const SizeOptions& opt) 
  : 
    bins(*this, n, 0, n-1),
    weights(*this, n, 1, 4),
    capacity(*this, 1, 100)
  {

    int _weights1[] = {4,3,1};
    IntArgs weights1(n, _weights1);

    for(int w = 0; w < n; w++) {
      rel(*this, weights[w] == weights1[w]);
    }

    for(int b = 0; b < n; b++) {
      IntVarArgs tmp;
      for(int j = 0; j < n; j++) {
        tmp << expr(*this, weights[j]*expr(*this, bins[j] == b));
      }

      rel(*this, sum(tmp) <= capacity);
    }

    rel(*this, capacity == 5);

    branch(*this, bins, INT_VAR_NONE(), INT_VAL_MIN()); 
    branch(*this, weights, INT_VAR_NONE(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "bins: " << bins << std::endl;
    os << "weights: " << weights << std::endl;
    os << "capacity: " << capacity << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  BinPacking(bool share, BinPacking& s) : Script(share,s) {
    bins.update(*this, share, s.bins);
    weights.update(*this, share, s.weights);
    capacity.update(*this, share, s.capacity);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BinPacking(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("BinPacking");
  opt.solutions(0);

  opt.search(BinPacking::SEARCH_DFS, "dfs");

  opt.parse(argc,argv);

  switch (opt.search()) {
    case BinPacking::SEARCH_DFS:
      Script::run<BinPacking,DFS,SizeOptions>(opt); break;
    }

  return 0;

}


