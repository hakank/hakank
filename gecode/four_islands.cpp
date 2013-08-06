/*
  
  Four Islands problem (Dell Logic Puzzles) in Gecode.

  http://brownbuffalo.sourceforge.net/FourIslandsClues.html
  """
  Title: Four Islands
  Author: Humphrey Dudley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1
  
  A tiny nation in the South Pacific contains four islands connected by bridges
  as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
  boasts a different primary export (alabaster, bananas, coconuts, and durian
  fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
  stadium, and koala preserve). Can you find the name, export, and tourist 
  attraction of each island on the map?
  
    N
  W   E     *compass directions
    S
  
  A, B, C, D are the islands
  
  (A) -- (B)
   |      |
   |      |
  (C) -- (D)
  
  
  1. The island noted for its koala preserve is due south of Pwana.
  2. The island with the largest alabaster quarry is due west of Quero.
  3. The island with the resort hotel is due east of the one that exports 
     durian fruit.
  4. Skern and the island with the jai alai stadium are connected by a 
     north-south bridge. 
  5. Rayou and the island that exports bananas are connected by an east-west
     bridge.
  6. The islands noted for the South Pacific's largest ice skating rink and 
     for the jai alai stadium are not connected by a bridge.
  
  Determine: Island island -- Island name -- Export -- Tourist Attraction
  """


  Compare with other models:
  * MiniZinc: http://www.hakank.org/minizinc/four_island.mzn 
  * Comet   : http://www.hakank.org/comet/four_islands.mzn 
  * ECLiPSE: http://www.hakank.org/eclipse/four_islands.mzn 
  * SICStus Prolog: http://www.hakank.org/sicstus/four_islands.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class FourIslands : public Script {
protected:

  const static int n = 4;

  IntVarArray island;
  IntVarArray exportx; // export
  IntVarArray attraction;

public:

  FourIslands(const Options& opt) 
    : 
    island(*this, n, 0, n-1),
    exportx(*this, n, 0, n-1),
    attraction(*this, n, 0, n-1)
  {

    enum { A, B, C, D };

    // island
    IntVar
      Pwana(island[0]), 
      Quero(island[1]), 
      Rayou(island[2]), 
      Skern(island[3]);


    // exportx
    IntVar 
      alabaster(exportx[0]), 
      bananas(exportx[1]), 
      coconuts(exportx[2]),
      durian_fruit(exportx[3]);

    // attraction
    IntVar
      resort_hotel(attraction[0]), 
      ice_skating_rink(attraction[1]), 
      jai_alai_stadium(attraction[2]), 
      koala_preserve(attraction[3]);


    distinct(*this, island);
    distinct(*this, exportx);
    distinct(*this, attraction);


    // 1. The island noted for its koala preserve is due south of Pwana.
    rel(*this, 
        (Pwana == A && koala_preserve == C)
        ||
        (Pwana == B && koala_preserve == D)
        );

    // 2. The island with the largest alabaster quarry is due west of Quero.
    rel(*this, 
        (alabaster == A && Quero == B) 
        || 
        (alabaster == C && Quero == D) 
        );
    
    // 3. The island with the resort hotel is due east of the one that exportxs 
    //    durian fruit.
    rel(*this, 
        ( durian_fruit == A && resort_hotel ==  B )
        ||
        ( durian_fruit == C && resort_hotel ==  D)
        );
    
    // 4. Skern and the island with the jai alai stadium are connected by a 
    //    north-south bridge. 
    rel(*this,
        (Skern == A && jai_alai_stadium == C) 
        ||
        (Skern == C && jai_alai_stadium == A) 
        ||
        (Skern == B && jai_alai_stadium == D) 
        ||
        (Skern == D && jai_alai_stadium == B) 
        );

    // 5. Rayou and the island that exportxs bananas are connected by an 
    //    east-west bridge.
    rel(*this,
        (Rayou == A && bananas == B) 
        ||
        (Rayou == B && bananas == A) 
        ||
        (Rayou == C && bananas == D) 
        ||
        (Rayou == D && bananas == C) 
        );

    // 6. The islands noted for the South Pacific's largest ice skating rink 
    //    and for the jai alai stadium are not connected by a bridge.
    rel(*this, 
        (ice_skating_rink == A && jai_alai_stadium == D)
        ||
        (ice_skating_rink == D && jai_alai_stadium == A)       
        ||
        (ice_skating_rink == B && jai_alai_stadium == C)
        ||
        (ice_skating_rink == C && jai_alai_stadium == B)
        );
    



    // branching
    branch(*this, island, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, exportx, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, attraction, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "island: " << island << endl;
    os << "export: " << exportx << endl;
    os << "attraction: " << attraction << endl;
  }


  // Constructor for cloning s
  FourIslands(bool share, FourIslands& s) : Script(share,s) {
    island.update(*this, share, s.island);
    exportx.update(*this, share, s.exportx);
    attraction.update(*this, share, s.attraction);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new FourIslands(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("FourIslands");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<FourIslands,DFS,Options>(opt);
    
  return 0;
}


