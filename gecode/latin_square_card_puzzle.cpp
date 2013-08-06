/*

  Latin square card puzzle in Gecode.

 
  Problem from Mario Livio's book about group theory
  "The Equation that couldn't be solved",
  page 22
  """
  "... Incidentally, you may get a kick out of solving this
  eighteenth century card puzzle: Arrange all the jacks,
  queens, kings, and aces from a deck of cards in a square so that 
  no suit or value would appear twice in any row, column, or the
  two main diagonals.
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/latin_square_card_puzzle.mzn

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;


class CardPuzzle : public Script {
protected:
  int n;          // size of the grid
  IntVarArray x;  // the grid


public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  // Symmetry options
  enum {
    SYMMETRY_NONE,
    SYMMETRY_MIN    // use symmetry breaking
  };

  CardPuzzle(const SizeOptions& opt) 
    : 
    n(opt.size()), 
    x(*this, n*n, 0, n*10) {

    

    int m = 10; // to divide and mod

    // First create the different valid values
    // which will be the domain of x.
    // 
    // E.g. for n = 4 the elements are
    //
    // values: i mod 10 
    //         0, 1, 2, 3,  suite 0: 0 div 10
    //        10,11,12,13,  suite 1: 1 div 10
    //        20,21,22,23,  suite 2: 2 div 10
    //        30,31,32,33   suite 3: 3 div 10

    IntArgs cards_a;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        cards_a << i+m*j;
      }
    }
    IntSet cards(cards_a);
    cout << "cards used: " << cards << endl;
    dom(*this, x, cards); // restrict the domain

   
    // Matrix wrapper for the x grid
    Matrix<IntVarArray> x_m(x, n, n);


    //
    // The constraints are now just a bunch of distinct constraints
    //

    // all values must be different
    distinct(*this, x, opt.icl());
  
    //
    // The constraints are now just a bunch of distinct constraints
    //

    // all values must be different
    distinct(*this, x, opt.icl());

    // divmod requires IntVar
    IntVar mm(*this, m, m);

    // rows and columns
    for(int i = 0; i < n; i++) {
      IntVarArgs rowmod(*this, n, cards);
      IntVarArgs rowdiv(*this, n, cards);
      IntVarArgs colmod(*this, n, cards);
      IntVarArgs coldiv(*this, n, cards);
      for(int j = 0; j < n; j++) {
        divmod(*this, x_m(j,i), mm, rowdiv[j], rowmod[j]);
        divmod(*this, x_m(i,j), mm, coldiv[j], colmod[j]);
      }
      distinct(*this, rowmod, opt.icl());
      distinct(*this, rowdiv, opt.icl());
      distinct(*this, colmod, opt.icl());
      distinct(*this, coldiv, opt.icl());
    }

    // diagonals
    IntVarArgs diag1mod(*this, n, cards);
    IntVarArgs diag1div(*this, n, cards);
    IntVarArgs diag2mod(*this, n, cards);
    IntVarArgs diag2div(*this, n, cards);
    for(int i = 0; i < n; i++) {
      divmod(*this, x_m(i,i), mm, diag1div[i], diag1mod[i]);
      divmod(*this, x_m(i, n-i-1), mm, diag2div[i], diag2mod[i]);
    }
    distinct(*this, diag1mod, opt.icl());
    distinct(*this, diag1div, opt.icl());    
    distinct(*this, diag2mod, opt.icl());
    distinct(*this, diag2div, opt.icl());


    // Symmetry breaking. 0 is upper left column
    if (opt.symmetry() == SYMMETRY_MIN) {
      rel(*this, x[0] == 0, opt.icl());
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_RANGE_MAX());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;
    int m = 10;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        os << (x[i*n+j].val() / m) << " of " << (x[i*n+j].val() % m) << "  ";
      }
      os << endl;
    }
    os << endl;
  }

  // Constructor for cloning s
  CardPuzzle(bool share, CardPuzzle& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CardPuzzle(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("CardPuzzle");

  opt.solutions(0);
  opt.size(4); // n: the size of the problem

  opt.search(CardPuzzle::SEARCH_DFS);
  opt.search(CardPuzzle::SEARCH_DFS, "dfs");
  opt.search(CardPuzzle::SEARCH_BAB, "bab");

  opt.symmetry(CardPuzzle::SYMMETRY_NONE);
  opt.symmetry(CardPuzzle::SYMMETRY_NONE, "none", "do not use symmetry");
  opt.symmetry(CardPuzzle::SYMMETRY_MIN, "min", "minimum element first");


  opt.parse(argc,argv);

  if (opt.size() > 10) {
    std::cerr << "Sorry, the maximum size is 10" << endl;
    return 1;
  }

  switch (opt.search()) {
    case CardPuzzle::SEARCH_DFS:
      MinimizeScript::run<CardPuzzle,DFS,SizeOptions>(opt); break;
    case CardPuzzle::SEARCH_BAB:
      MinimizeScript::run<CardPuzzle,BAB,SizeOptions>(opt); break;
    }

  return 0;
}


