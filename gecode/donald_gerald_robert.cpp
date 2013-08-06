/*
  DONALD + GERALD = ROBERT in Gecode.

  Famous alphametic puzzle.

  This is a study how to create different strategy options in Gecode, and play
  with them.


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my other Gecode models at http://www.hakank.org/gecode/ .

 */


/*
 Note: This model relies heavily on the SEND+MORE=MONEY example (money.cpp) 
       from the Gecode distribution. The carry model is from donald.cpp 
       (which I found later).

       Here I experiment with different branching strategies,
       search engines, and models.

       Search engines:
        -model (single, carry) default: carry
                model variants
                  single: use single linear equation
                  carry: use carry

       Variable branching: The names and their mappings is from the
                           the mappings Gecode/FlatZinc does for MiniZinc's 
                           branching options.
       -branching (input-order, first-fail, anti-first-fail, smallest, largest, occurrence, max-regret, var-min-max) default: smallest
                branching variants
                  input-order: use VAR_NONE
                  first-fail: use VAR_SIZE_MIN
                  anti-first-fail: use VAR_SIZE_MAX
                  smallest: use VAR_MIN_MIN
                  largest: use VAR_MAX_MAX
                  occurrence: use VAR_DEGREE_MAX
                  max-regret: use VAR_REGRET_MIN_MAX
                  var-min-max: use VAR_MIN_MAX
 
        -search (dfs, lds) default: dfs
                search engine variants
                  dfs: use DFS as search engine
                  
*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class DonaldGeraldRobert : public Script {
protected:

  // Number of letters
  static const int nl = 10;

  // Array of letters
  IntVarArray x;

public:

  // Model variants
  enum {
    MODEL_SINGLE, ///< Use single linear equation
    MODEL_CARRY   ///< Use carries
  };

  // Search engines
  enum {
    SEARCH_DFS,
  };

  // Actual model
  DonaldGeraldRobert(const Options& opt) : x(*this, nl, 0, 9) {

    IntVar d(x[0]), o(x[1]), n(x[2]), a(x[3]), l(x[4]), 
           g(x[5]), e(x[6]), r(x[7]), b(x[8]),t(x[9]) ;

    rel(*this, d > 0, opt.icl()); 
    rel(*this, g > 0, opt.icl());
    rel(*this, r > 0, opt.icl());

    distinct(*this, x);

    // Which model to use?
    switch(opt.model()) {
    case MODEL_SINGLE: // plain model
      rel(*this, 
             100000*d + 10000*o + 1000*n + 100*a + 10*l + d
           + 100000*g + 10000*e + 1000*r + 100*a + 10*l + d
          == 100000*r + 10000*o + 1000*b + 100*e + 10*r + t, opt.icl());
      break;
      
    case MODEL_CARRY: {
      // Using carry. This is from the donald.cpp model
      IntVar c0(*this,0,1), c1(*this,0,1), c2(*this,0,1),
        c3(*this,0,1), c4(*this,0,1);
      rel(*this,    d+d == t+10*c0, opt.icl());
      rel(*this, c0+l+l == r+10*c1, opt.icl());
      rel(*this, c1+a+a == e+10*c2, opt.icl());
      rel(*this, c2+n+r == b+10*c3, opt.icl());
      rel(*this, c3+o+e == o+10*c4, opt.icl());
      rel(*this, c4+d+g == r,       opt.icl());       
    }
      break;
      
    }

    // Use the IntVarBranch option from the command line -branching
    // This was suggested by Mikael Zayenz Lagerkvist.
    branch(*this, x, 
           static_cast<IntVarBranch>(opt.branching()),
           INT_VAL_MAX());
    
  } 
  
  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << x << std::endl;
  }

  // Constructor for cloning a s
  DonaldGeraldRobert(bool share, DonaldGeraldRobert& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new DonaldGeraldRobert(share,*this);
  }

};


int
main(int argc, char* argv[]) {

  Options opt("DONALD + GERALD = ROBERT");

  opt.solutions(1);
  opt.iterations(2000);
  opt.icl(ICL_DOM); // default is domain consistency

  opt.model(DonaldGeraldRobert::MODEL_CARRY);
  opt.model(DonaldGeraldRobert::MODEL_SINGLE, "single", "use single linear equation");
  opt.model(DonaldGeraldRobert::MODEL_CARRY, "carry", "use carry");

  opt.search(DonaldGeraldRobert::SEARCH_DFS);
  opt.search(DonaldGeraldRobert::SEARCH_DFS, "dfs", "use DFS as search engine");

  // The option names is from Gecode/FlatZinc mapping of the the MiniZinc
  // branch options.
  /*
  opt.branching(INT_VAR_SIZE_MIN); // first_fail is default
  opt.branching(INT_VAR_NONE, "input-order", "use VAR_NONE");
  opt.branching(INT_VAR_SIZE_MIN, "first-fail", "use VAR_SIZE_MIN");
  opt.branching(INT_VAR_SIZE_MAX, "anti-first-fail", "use VAR_SIZE_MAX");
  opt.branching(INT_VAR_MIN_MIN, "smallest", "use VAR_MIN_MIN");
  opt.branching(INT_VAR_MAX_MAX, "largest", "use VAR_MAX_MAX");
  opt.branching(INT_VAR_DEGREE_MAX, "occurrence", "use VAR_DEGREE_MAX");
  opt.branching(INT_VAR_REGRET_MIN_MAX, "max-regret", "use VAR_REGRET_MIN_MAX");
  opt.branching(INT_VAR_MIN_MAX, "var-min-max", "use VAR_MIN_MAX");
  */

  opt.parse(argc,argv);

  switch (opt.search()) {
  case DonaldGeraldRobert::SEARCH_DFS: 
    Script::run<DonaldGeraldRobert,DFS,Options>(opt); break;
  }

  return 0;
}

