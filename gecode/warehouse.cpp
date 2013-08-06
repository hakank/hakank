/*  

  Warehouse location problem in Gecode.

  From OPL model warehouse.mod

  Compare with the following models:
  * Comet: http://www.hakank.org/comet/warehouse.co
  * http://www.hakank.org/eclipse/warehouse.ecl
  * http://www.hakank.org/sicstus/warehouse.pl

  Note: There is a warehouses model in the Gecode distribution
  that solves the same problem, and is also used as case study example in 
  "Modeling and Programming with Gecode" (which I read some month ago).

  However, in this model I have translated the Comet model rather 
  faithfully without looking at the distributed model.

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class Warehouse : public MinimizeScript {
protected:

  static const int num_warehouses = 5; 
  static const int num_stores = 10; 

  BoolVarArray Open;
  BoolVarArray Supply;
  IntVar Total;  // total cost (to minimize)

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  Warehouse(const SizeOptions& opt) 
  :    
    Open(*this, num_warehouses, 0, 1),
    Supply(*this, num_stores*num_warehouses, 0,1),
    Total(*this, 0, Int::Limits::max)
  {

    int Fixed = 30; // fixed cost for Open

    int _Capacity[] = {1,4,2,1,3};
    IntArgs Capacity(num_warehouses, _Capacity);

    int _SupplyCost[] = {
      20, 24, 11, 25, 30, 
      28, 27, 82, 83, 74,
      74, 97, 71, 96, 70,
       2, 55, 73, 69, 61,
      46, 96, 59, 83,  4,
      42, 22, 29, 67, 59,
       1,  5, 73, 59, 56,
      10, 73, 13, 43, 96,
      93, 35, 63, 85, 46,
      47, 65, 55, 71, 95  
    };
    IntArgs SupplyCost(num_warehouses*num_stores, _SupplyCost);

    IntVarArgs FixedSumArr;
    for(int w = 0; w < num_warehouses; w++) {
      FixedSumArr << expr(*this, Fixed*Open[w]);
    }
    
    IntVarArgs SumArr;
    for(int w = 0; w < num_warehouses; w++) {
      for(int s = 0; s < num_stores; s++) {
        SumArr << expr(*this, SupplyCost[s*num_warehouses+w] * Supply[s*num_warehouses+w]);
      }
    }
    rel(*this, Total == sum(FixedSumArr) + sum(SumArr));

    for(int s = 0; s < num_stores; s++) {
      BoolVarArgs tmp;
      for(int w = 0; w < num_warehouses; w++) {
        tmp << expr(*this, Supply[s*num_warehouses+w]==1);
      }
      rel(*this, sum(tmp) == 1);
    }

    for(int w = 0; w < num_warehouses; w++) {
      for(int s = 0; s < num_stores; s++) {
        rel(*this, Supply[s*num_warehouses+w] <= Open[w]);
      }
    }

    for(int w = 0; w < num_warehouses; w++) {
      IntVarArray tmp(*this, num_stores, 0, 10000);
      for(int s = 0; s < num_stores; s++) {
        rel(*this, tmp[s] == Supply[s*num_warehouses+w]);
      }
      rel(*this, sum(tmp) <= Capacity[w]);
    }

    // check optimal solutions
    if (opt.search() == SEARCH_DFS) {
      rel(*this, Total == 383);
    }

    //
    // Branching.
    //
    branch(*this, Open, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, Supply, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Total: " << Total << endl;
    os << "Open: " << Open << endl;
    os << "Supply: " << endl;
    Matrix<BoolVarArray> m(Supply, num_warehouses, num_stores);
    os << m << endl;
    os << endl;
  }

  // Constructor for cloning s
  Warehouse(bool share, Warehouse& s) : MinimizeScript(share,s) {
    Open.update(*this, share, s.Open);
    Supply.update(*this, share, s.Supply);
    Total.update(*this, share, s.Total);
  }

  // Return cost
  virtual IntVar cost(void) const {
    return Total;
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Warehouse(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Warehouse");
  opt.solutions(0);

  opt.search(Warehouse::SEARCH_BAB);
  opt.search(Warehouse::SEARCH_DFS, "dfs");
  opt.search(Warehouse::SEARCH_BAB, "bab");
 
  opt.parse(argc,argv);

  switch (opt.search()) {
    case Warehouse::SEARCH_DFS:
      MinimizeScript::run<Warehouse,DFS,SizeOptions>(opt); break;
    case Warehouse::SEARCH_BAB:
      MinimizeScript::run<Warehouse,BAB,SizeOptions>(opt); break;
    }

  return 0;

}


