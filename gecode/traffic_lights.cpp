/*

  Traffic lights problem in Gecode.

  CSPLib problem 16
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
  """
  Specification:
  Consider a four way traffic junction with eight traffic lights. Four of the traffic 
  lights are for the vehicles and can be represented by the variables V1 to V4 with domains 
  {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are 
  for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.
  
  The constraints on these variables can be modelled by quaternary constraints on 
  (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples 
  {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.
 
  It would be interesting to consider other types of junction (e.g. five roads 
  intersecting) as well as modelling the evolution over time of the traffic light sequence. 
  ...
 
  Results
  Only 2^2 out of the 2^12 possible assignments are solutions.
  
  (V1,P1,V2,P2,V3,P3,V4,P4) = 
     {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
     [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}
 
 
  The problem has relative few constraints, but each is very tight. Local propagation 
  appears to be rather ineffective on this problem.
    
  """
  
  Compare with other models:
  * MiniZinc: http://www.hakank.org/minizinc/traffic_lights.mzn
  * Comet: http://www.hakank.org/comet/traffic_lights.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;
using std::string;

class TrafficLights : public Script {
protected:
  /// Number of variables
  static const int n = 4;

  IntVarArray V; // vehicles
  IntVarArray P; // pedestrians

public:

  /// Actual model
  TrafficLights(const Options& opt) : 
    V(*this, n, 1, n),
    P(*this, n, 1, n) 
  {

    int r  = 1; // red
    int ry = 2; // red-yellow
    int g  = 3; // green
    int y  = 4; // yellow
    
    /*
    int _allowed[] = 
      {
        r,r,g,g, 
        ry,r,y,r, 
        g,g,r,r, 
        y,r,ry,r
      };
    IntArgs allowed(n*n, _allowed);
    */

    TupleSet t2;
    t2.add(IntArgs(4, r,r,g,g));
    t2.add(IntArgs(4, ry,r,y,r));
    t2.add(IntArgs(4, g,g,r,r)); 
    t2.add(IntArgs(4, y,r,ry,r));
    t2.finalize();


    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (j == (i+1) % n) {
          // Table constraint
          extensional(*this, IntVarArgs() << V[i] << P[i] << V[j] << P[j], t2);
        }
      }
    }

    branch(*this, V, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, P, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {
    string s[] = {"", "r","ry","g","y"};
    for(int i = 0; i < n; i++) {
      os << V[i] << " " << P[i] << " ";
    }
    os << std::endl;
    for(int i = 0; i < n; i++) {
      os << s[V[i].val()] << " " << s[P[i].val()] << " ";
    }
    os << std::endl;
    os << std::endl;

  }

  TrafficLights(bool share, TrafficLights& s) : Script(share,s) {
    V.update(*this, share, s.V);
    P.update(*this, share, s.P);
  }

  /// Copy during cloning
  virtual Space*
  copy(bool share) {
    return new TrafficLights(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("TrafficLights");
  opt.solutions(0);
  opt.parse(argc,argv);
  Script::run<TrafficLights,DFS,Options>(opt);
  return 0;
}
