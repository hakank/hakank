/*

  Einstein puzzle in Gecode.

  This is a Gecode version of the OPL code presented in 
  Daniel Selman's "Einstein's Puzzle - Or, 'The Right Tool for the Job'"
  http://blogs.ilog.com/brms/2009/02/16/einsteins-puzzle/
  """
  * Norwegian cats water Dunhill yellow
  * Dane horses tea Blends blue
  * Brit birds milk Pall Mall red
  * German fish coffee Prince green
  * Sweed dogs beer Blue Master white
  """

  Note: I let the "Sweed" stand, it should - of course - be "Swede".

  See 
  * http://www.stanford.edu/~laurik/fsmbook/examples/Einstein%27sPuzzle.html

  Compare with the following models:
  * Comet: http://hakank.org/comet/einstein_opl.co
  * MiniZinc: http://hakank.org/minizinc/einstein_opl.mzn

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;


class Einstein : public Script {
protected:

  enum States  { Brit, Sweed, Dane, Norwegian, German};
  enum Animals { dogs, fish, birds, cats, horses};
  enum Drinks  { tea, coffee, milk, beer, water};
  enum Smoke   { Pall_Mall, Dunhill, Blends, Blue_Master, Prince };
  enum Colors  { red, green, white, yellow, blue};
  
  static const int n = 5; // number of houses, persons, etc

  IntVarArray s; // states
  IntVarArray a; // animals
  IntVarArray d; // drinks
  IntVarArray k; // smoKes
  IntVarArray c; // colors

public:


  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
    SEARCH_RESTART, // Use restart to optimize
  };

  Einstein(const Options& opt) 
  : 
    s(*this, n, 1, n), 
    a(*this, n, 1, n),
    d(*this, n, 1, n),
    k(*this, n, 1, n),
    c(*this, n, 1, n)
  {

    // all different
    distinct(*this, s, opt.icl());
    distinct(*this, a, opt.icl());
    distinct(*this, d, opt.icl());
    distinct(*this, k, opt.icl());
    distinct(*this, c, opt.icl());

    
    //
    // the clues (see above)
    //
   
    // 1. The Brit lives in a red house.
    rel(*this, s[Brit] == c[red]);
    // 2. The Swede keeps dogs as pets.
    rel(*this, s[Sweed] == a[dogs]);
    // 3. The Dane drinks tea.
    rel(*this, s[Dane] == d[tea]);
    // 5. The owner of the Green house drinks coffee.
    rel(*this, c[green] == d[coffee]);
    // 6. The person who smokes Pall Mall rears birds.
    rel(*this, k[Pall_Mall] == a[birds]);
    // 7. The owner of the Yellow house smokes Dunhill.
    rel(*this, c[yellow] == k[Dunhill]);
    // 8. The man living in the centre house drinks milk.
    rel(*this, d[milk] == 3);
    // 9. The Norwegian lives in the first house.
    rel(*this, s[Norwegian] == 1);
    // 12. The man who smokes Blue Master drinks beer.
    rel(*this, k[Blue_Master] == d[beer]);
    // 13. The German smokes Prince.
    rel(*this, s[German] == k[Prince]);
    // 4. The Green house is on the left of the White house.
    rel(*this, c[green] == c[white]-1);
    // 10. The man who smokes Blends lives next to the one who keeps cats.
    rel(*this, abs(k[Blends] - a[cats]) == 1);
    // 11. The man who keeps horses lives next to the man who smokes Dunhill.
    rel(*this, abs(a[horses] - k[Dunhill]) == 1);
    // 14. The Norwegian lives next to the blue house.
    rel(*this, abs(s[Norwegian] - c[blue]) == 1);
    // 15. The man who smokes Blends has a neighbour who drinks water.
    rel(*this, abs(k[Blends] - d[water]) == 1);


    branch(*this, s, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, a, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, d, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, k, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, c, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

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
    os << "states : " << s << endl;
    os << "animals: " << a << endl;
    os << "drinks : " << d << endl;
    os << "smokes : " << k  << endl;
    os << "colors : " << c  << endl;
    os << endl;

  }

  // Constructor for cloning s
  Einstein(bool share, Einstein& ss) : Script(share, ss) {
    s.update(*this, share, ss.s);
    a.update(*this, share, ss.a);
    d.update(*this, share, ss.d);
    k.update(*this, share, ss.k);
    c.update(*this, share, ss.c);
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Einstein(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("Einstein");

  opt.solutions(0);
  opt.icl(ICL_BND);
  opt.parse(argc,argv);
  Script::run<Einstein,DFS,Options>(opt); 

  return 0;

}


