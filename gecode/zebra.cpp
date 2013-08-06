/*

  Zebra puzzle in Gecode.

  This is a standard problem in constraint programming (and puzzle solving in general).

  This model was inspired by the MiniZinc model
  http://www.g12.csse.unimelb.edu.au/minizinc/downloads/examples-latest/zebra.mzn
  """
  WHO OWNS THE ZEBRA?
  
  This is a puzzle which has been circulating the net. There are a couple
  different versions out there which try to be a little more politically
  correct but are essentially the same problem.    
  	1. There are five houses, each of a different color and inhabited by
 	   men of different nationalities, with different pets, drinks,
 	   and cigarettes.
  	2. The Englishman lives in the red house.
  	3. The Spaniard owns the dog.
  	4. Coffee is drunk in the green house.
  	5. The Ukrainian drinks tea.
  	6. The green house is immediately to the right of the ivory house.
  	7. The Old Gold smoker owns snails.
  	8. Kools are smoked in the yellow house.
  	9. Milk is drunk in the middle house.
  	10. The Norwegian lives in the first house on the left.
  	11. The man who smokes Chesterfields lives in the house next to the
 	    man with the fox.
  	12. Kools are smoked in the house next to the house where the horse is
 	    kept.
  	13. The Lucky Strike smoker drinks orange juice.
  	14. The Japanese smoke Parliaments.
  	15. The Norwegian lives next to the blue house.
  NOW, who drinks water? And who owns the zebra?

  MiniZinc Version
  Peter Stuckey September 30 2006
  """


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;

/*

  Solution:

  nation: {3, 4, 2, 1, 5}
  colour: {3, 5, 4, 1, 2}
  animal: {4, 1, 2, 5, 3}
  drink : {5, 2, 3, 4, 1}
  smoke : {3, 1, 2, 4, 5}

  which is the same as the MiniZinc model here:
  http://www.g12.csse.unimelb.edu.au/minizinc/downloads/examples-latest/zebra.soln

 */


class ZebraPuzzle : public Script {
protected:

  enum Nation { English, Spanish, Ukrainian, Norwegian, Japanese };
  enum Color  { Red, Green, Ivory, Yellow, Blue };
  enum Animal { Dog, Fox, Horse, Zebra, Snails };
  enum Drink  { Coffee, Tea, Milk, OrangeJuice, Water };
  enum Smoke  { OldGold, Kools, Chesterfields,  LuckyStrike, Parliaments };


  static const int num_p = 5; // number of houses, persons, etc

  IntVarArray nation;
  IntVarArray colour;
  IntVarArray animal;
  IntVarArray drink;
  IntVarArray smoke;

public:



  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
    SEARCH_RESTART, // Use restart to optimize
  };

  ZebraPuzzle(const Options& opt) 
  : 
    nation(*this, num_p, 1, num_p), 
    colour(*this, num_p, 1, num_p),
    animal(*this, num_p, 1, num_p),
    drink(*this,  num_p, 1, num_p),
    smoke(*this,  num_p, 1, num_p)

  {
    
    //
    // the clues (see above)
    //
    rel(*this, nation[English] == colour[Red], opt.icl());
    rel(*this, nation[Spanish] == animal[Dog], opt.icl());
    rel(*this, drink[Coffee] == colour[Green], opt.icl()); 
    rel(*this, nation[Ukrainian] == drink[Tea], opt.icl());
    rel(*this, colour[Green] == colour[Ivory] + 1, opt.icl()); // colour[Green] rightof colour[Ivory]
    rel(*this, smoke[OldGold] == animal[Snails], opt.icl());
    rel(*this, smoke[Kools] == colour[Yellow], opt.icl());    
    rel(*this, drink[Milk] == 3, opt.icl()); // middle(drink[Milk])
    rel(*this, nation[Norwegian] == 1, opt.icl()); // left(nation[Norwegian])
    next_to(*this, smoke[Chesterfields], animal[Fox], opt.icl());
    next_to(*this, smoke[Kools], animal[Horse], opt.icl());
    rel(*this, smoke[LuckyStrike] == drink[OrangeJuice], opt.icl());
    rel(*this, nation[Japanese] == smoke[Parliaments], opt.icl());
    next_to(*this, nation[Norwegian], colour[Blue], opt.icl());
    

    // all different
    distinct(*this, nation, opt.icl());
    distinct(*this, colour, opt.icl());
    distinct(*this, animal, opt.icl());
    distinct(*this, drink, opt.icl());
    distinct(*this, smoke, opt.icl());

    branch(*this, nation, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, colour, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, animal, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, drink, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, smoke, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  /**
   * next_to(*this, x, y, opt.icl()): x is next to y 
   *
   * (just to simplify the model somewhat)
   *
   */
  void next_to(Space & space, IntVar x, IntVar y, IntConLevel icl = ICL_DOM) {
    
    // rel(space, abs(space, minus(space, x, y, icl), icl) == 1, icl); 
    rel(space, abs(x-y) == 1, icl); 
    
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "nation: " << nation << std::endl;
    os << "colour: " << colour << std::endl;
    os << "animal: " << animal << std::endl;
    os << "drink : " << drink  << std::endl;
    os << "smoke : " << smoke  << std::endl;

    os << std::endl;

  }

  // Constructor for cloning s
  ZebraPuzzle(bool share, ZebraPuzzle& s) : Script(share,s) {
    nation.update(*this, share, s.nation);
    colour.update(*this, share, s.colour);
    animal.update(*this, share, s.animal);
    drink.update(*this, share, s.drink);
    smoke.update(*this, share, s.smoke);
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ZebraPuzzle(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("ZebraPuzzle");

  opt.solutions(0);
  opt.icl(ICL_BND);
  opt.parse(argc,argv);
  Script::run<ZebraPuzzle,DFS,Options>(opt); 

  return 0;

}


