/*
  
  3 jugs problem in Gecode.

  Problem from 
  Taha "Introduction to Operations Research", page 245f .

  This is a constraint programming approach (or at least with a 
  more CP bent), as the MIP approach in the MiniZinc model:
  http://www.hakank.org/minzinc/3jugs.mzn


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/3jugs2.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/3jugs2.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/set.hh>


using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;

class ThreeJugs : public MinimizeScript {

protected:

  static const int n = 15;
  static const int start = 0; // first (0-based)
  static const int end = 14; // last (0-based)
  static const int M = 99; // a very large number

  IntVarArray rhs;     // indicating start/end nodes
  IntVarArray x;       // the resulting connection matrix
  IntVarArray outFlow; // out flow array
  IntVarArray inFlow;  // in flow array
  IntVar total_cost;   // total cost, to minimize
  
public:

  ThreeJugs(const Options& opt) 
    : 
    rhs(*this, n, -1, 1),
    x(*this, n*n, 0, 1),
    outFlow(*this, n, 0, 1),
    inFlow(*this, n, 0, 1),
    total_cost(*this, 0, 99) // very large number
  {

    // distances
    int _d[] = 
      {
        M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M,
        M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M,
        M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M,
        M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M,
        M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M,
        M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M,
        M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M,
        M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
        M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M,
        M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M,
        M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M,
        M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M,
        M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M,
        M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
        M, M, M, M, M, M, M, M, M, M, M, M, M, M, M
      };

    IntArgs d(n*n, _d);


    // total_cost
    IntVarArgs total_cost_tmp;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        int v = d[i*n+j];
        if (d[i*n+j] < M) {
          total_cost_tmp << expr(*this, v*x[i*n+j]);
        }
      }
    }
    rel(*this, total_cost == sum(total_cost_tmp));


    // set rhs for start/end nodes
    for(int i = 0; i < n; i++) {
      if (i == start) {
        rel(*this, rhs[i] == 1);
      } else if (i == end) {
        rel(*this, rhs[i] == -1);
      } else {
        rel(*this, rhs[i] == 0);
      }
    }

    // calculate out flow
    for(int i = 0; i < n; i++) {
      IntVarArgs outFlowTmp;
      for(int j = 0; j < n; j++) {
        if (d[i*n+j] < M) {
          outFlowTmp << x[i*n+j];
        }
      }
      rel(*this, outFlow[i] == sum(outFlowTmp));
    }

    // calculate in flow
    for(int j = 0; j < n; j++) {
      IntVarArgs inFlowTmp;
      for(int i = 0; i < n; i++) {
        if (d[i*n+j] < M) {
          inFlowTmp << x[i*n+j];
        }
      }
      rel(*this, inFlow[j] == sum(inFlowTmp));
    }

    // outflow = inflow
    for(int i = 0; i < n; i++) {
      rel(*this, rhs[i] == outFlow[i] - inFlow[i]);

      // no loops
      rel(*this, x[i*n+i] == 0);
    }


    // sanity: there can be no connection in x if there 
    // is not connection in d
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (d[i*n+j] == M) {
          rel(*this, x[i*n+j] == 0);
        }
      }
    }

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "rhs       : " << rhs << endl;
    os << "outFlow   : " << outFlow << endl;
    os << "inFlow    : " << inFlow << endl;
    os << "total_cost: " << total_cost << endl;
    // os << "x: " << x << endl;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        cout << x[i*n+j] << " ";
      }
      cout << endl;
    }
    os << endl;
  }


  // Constructor for cloning s
  ThreeJugs(bool share, ThreeJugs& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    rhs.update(*this, share, s.rhs);
    outFlow.update(*this, share, s.inFlow);
    inFlow.update(*this, share, s.outFlow);
    total_cost.update(*this, share, s.total_cost);
  }


  // Return cost
  virtual IntVar cost(void) const {
    return total_cost;
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ThreeJugs(share,*this);
  }

  

};


int
main(int argc, char* argv[]) {

  Options opt("ThreeJugs");
  opt.solutions(0);
  opt.parse(argc,argv);

  MinimizeScript::run<ThreeJugs,BAB,Options>(opt);    

  return 0;
}


