/*

  Set covering in Gecode.

  Placing of firestations, from Winston "Operations Research", page 486.

  The objective is to place minimum number of fire stations so that all
  cities are within a minimum distance from a fire station, i.e. that
  the fire stations "cover" all the cities.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering.mzn
  * Comet: http://www.hakank.org/comet/set_covering.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class SetCovering : public MinimizeScript {
protected:

  // minimum distance required 
  // from opt.size(), defaults to 15
  int min_distance;

  // number of cities
  static const int num_cities = 6;

  // cities: where to place the fire stations
  IntVarArray x;   
  // number of of fire stations to place (to be minimized)
  IntVar z;


public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  SetCovering(const SizeOptions& opt) 
  : 
    min_distance(opt.size()),
    x(*this, num_cities, 0, 1),
    z(*this, 0, num_cities)
  {

    // distance between the cities
    int distance[] =
      {
        0,10,20,30,30,20,
       10, 0,25,35,20,10,
       20,25, 0,15,30,20,
       30,35,15, 0,15,25,
       30,20,30,15, 0,14,
       20,10,20,25,14, 0
      };

    // z = sum of placed fire stations
    linear(*this, x, IRT_EQ, z, opt.icl());

    // ensure that all cities are covered by at least one fire station
    for(int i = 0; i < num_cities; i++) {

      IntArgs in_distance(num_cities);  // the cities within the distance
      for(int j = 0; j < num_cities; j++) {
        if (distance[i*num_cities+j] <= min_distance) {
          in_distance[j] = 1;
        } else {
          in_distance[j] = 0;
        }
      }

      linear(*this, in_distance, x, IRT_GQ, 1, opt.icl());
    }
    
    branch(*this, x, INT_VAR_SIZE_MAX(), INT_VAL_SPLIT_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "z: " << z << std::endl;
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  SetCovering(bool share, SetCovering& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
  }

  // Return cost
  virtual IntVar cost(void) const {
    return z;
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SetCovering(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("SetCovering");
  opt.solutions(0);

  opt.search(SetCovering::SEARCH_BAB);
  opt.search(SetCovering::SEARCH_DFS, "dfs");
  opt.search(SetCovering::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (!opt.size()) {
    opt.size(15); // default minimum distance
  }
  switch (opt.search()) {
    case SetCovering::SEARCH_DFS:
      MinimizeScript::run<SetCovering,DFS,SizeOptions>(opt); break;
    case SetCovering::SEARCH_BAB:
      MinimizeScript::run<SetCovering,BAB,SizeOptions>(opt); break;
    }

  return 0;

}


