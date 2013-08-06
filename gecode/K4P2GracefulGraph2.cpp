/*
  
  K4P2 Graceful Graph in Gecode.

  Problem from Minion summer_school/examples/K4P2GracefulGraph.eprime

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/K4P2GracefulGraph2.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/K4P2GracefulGraph2.pl
  * Comet: http://www.hakank.org/comet/K4P2GracefulGraph2.co


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


class GracefulGraph : public Script {

protected:

  static const int m = 16;
  static const int n = 8;
  IntVarArray nodes;
  IntVarArray edges;
  
  
public:

  GracefulGraph(const Options& opt) 
    : 
    nodes(*this, n, 0, m),
    edges(*this, m, 0, m)

  {

    //
    // The graph. Note: 1-based.
    //
    int graph[] = {
      1, 2,
      1, 3,
      1, 4,
      2, 3,
      2, 4,
      3, 4,
      
      5, 6,
      5, 7,
      5, 8,
      6, 7,
      6, 8,
      7, 8,
      
      1, 5,
      2, 6,
      3, 7,
      4, 8
    };
    
    distinct(*this, edges);
    distinct(*this, nodes);

    for(int i = 0; i < m; i++) {
      int g1 = graph[i*2+0]-1;
      int g2 = graph[i*2+1]-1;

      rel(*this, edges[i] == abs(nodes[g1] - nodes[g2])); 
    }

    // branching
    branch(*this, nodes, INT_VAR_AFC_SIZE_MIN(), INT_VAL_MAX());
    // branch(*this, edges, INT_VAR_SIZE_MIN(), INT_VAL_MAX());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "edges: " << edges << endl;
    os << "nodes: " << nodes << endl;
    os << endl;
  }


  // Constructor for cloning s
  GracefulGraph(bool share, GracefulGraph& s) : Script(share,s) {
    edges.update(*this, share, s.edges);
    nodes.update(*this, share, s.nodes);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new GracefulGraph(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("GracefulGraph");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<GracefulGraph,DFS,Options>(opt);    

  return 0;
}


