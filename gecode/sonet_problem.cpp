/*

  SONET problem in Gecode.


  Translation of the ESSENCE' model in the Minion Translator examples:
  http://www.cs.st-andrews.ac.uk/~andrea/examples/sonet/sonet_problem.eprime
  """
  The SONET problem is a network design problem: set up a network between
  n nodes, where only certain nodes require a connection.
  Nodes are connected by putting them on a ring, where all nodes
  on a ring can communicate. Putting a node on a ring requires a so-called
  ADM, and each ring has a capacity of nodes, i.e. ADMs. There is a certain 
  amount of rings, r, that is available. The objective is to set up a network
  by using a minimal amount of ADMs.


  About the problem model

  The problem model has the amount of rings ('r'), amount of nodes('n'),
  the 'demand' (which nodes require communication) and node-capacity of each 
  ring ('capacity_nodes') as parameters.
  The assignement of nodes to rings is modelled by a 2-dimensional matrix 'rings',
  indexed by the amnount of rings and nodes. The matrix-domain is boolean:
  If the node in column j is assigned to the ring in row i, then rings[i,j] = 1 
  and 0 otherwise. So all the '1's in the matrix 'rings' stand for an ADM.
  Hence the objective is to minimise the sum over all columns and rows of matrix
  'rings'.
  """


  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/sonet_problem.mzn
  * Comet   : http://www.hakank.org/comet/sonet_problem.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

/*

  Answer should be:
  z: 7
  0 1 1 0 1
  1 0 0 1 0
  1 1 0 0 0
  0 0 0 0 0

  If the option 

    -search dfs

  is set then all 6 solutions are generated.  

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class Sonet : public MinimizeScript {
protected:

                           // size of problem
  static const int r = 4;  // upper bound for amount of rings
  static const int n = 5;  // amount of clients

  IntVarArray rings;       // rings
  IntVar z;                // number of rings, the objective to minimize

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  }
;

  Sonet(const Options& opt) 
    : 
    rings(*this, r*n, 0, 1),
    z(*this, 0, 10)
  {


    // original comment: 
    // """
    // we have double entries here because of the symmetric structure!
    // """
    int demand[] =
      {
       0,1,0,1,0,
       1,0,1,0,0,
       0,1,0,0,1,
       1,0,0,0,0,
       0,0,1,0,0
      };

    int capacity_nodes[] = {3,2,2,1};

    linear(*this, rings, IRT_EQ, z, opt.icl());
 
    Matrix<IntVarArray> rings_m(rings, n, r);

    // original comment:
    // """
    // if there is a demand between 2 nodes, then there has to exist 
    // a ring, on which they are both installed
    // """
    for(int client1 = 0; client1 < n; client1++) {
      for(int client2 = client1 + 1; client2 < n; client2++) {
        if(demand[client1*n+client2] == 1) {
          // ESSENCE' code:
          //   exists ring : int(1..r) .
          //      rings[ring,client1] + rings[ring, client2] >= 2)

          // here we use reification: 
          BoolVarArray bs(*this, r, 0, 1);
          for(int ring = 0; ring < r; ring++) {
            // does this ring satisfy the condition?
            // note: the "~" is needed
            bs[ring] = expr(*this, (rings_m(client1, ring) + rings_m(client2,ring) >= 2), opt.icl());
          }
          // at least one success is needed
          linear(*this, bs, IRT_GQ, 1, opt.icl());
        }
      }
    }
    
 
    // original comment:
    // """
    // capacity of each ring must not be exceeded     
    // """
    for(int ring = 0; ring < r; ring++) {
      linear(*this, rings_m.row(ring), IRT_LQ, capacity_nodes[ring], opt.icl());
    }

    //
    // If 
    //   -search dfs 
    // we show all optimal solutions, i.e. for z == 7
    //
    if (opt.search() == SEARCH_DFS) {
      rel(*this, z == 7, opt.icl());
    }

  
    // branching
    branch(*this, rings, INT_VAR_DEGREE_SIZE_MAX(), INT_VAL_SPLIT_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "z: " << z << std::endl;
    for(int i = 0; i < r; i++) {
      for(int j = 0; j < n; j++) {
        os << rings[i*n+j] << " ";
      }
      os << std::endl;
    }
    os << std::endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return z;
  }


  // Constructor for cloning s
  Sonet(bool share, Sonet& s) : MinimizeScript(share,s) {
    rings.update(*this, share, s.rings);
    z.update(*this, share, s.z);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Sonet(share,*this);

  }
};


int
main(int argc, char* argv[]) {

  Options opt("Sonet");

  opt.solutions(0);
  opt.icl(ICL_BND);
  opt.search(Sonet::SEARCH_BAB);
  opt.search(Sonet::SEARCH_DFS, "dfs");
  opt.search(Sonet::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (opt.search() == Sonet::SEARCH_DFS) {
    Script::run<Sonet,DFS,Options>(opt);
  } else {
    MinimizeScript::run<Sonet,BAB,Options>(opt);
  }


  return 0;
}


