/*

  Fill-a-Pix problem in Gecode.
 
  From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
  """
  Each puzzle consists of a grid containing clues in various places. The 
  object is to reveal a hidden picture by painting the squares around each 
  clue so that the number of painted squares, including the square with 
  the clue, matches the value of the clue. 
  """

  Other names of this puzzle:

      * ぬり絵パズル
      * Nurie-Puzzle
      * Majipiku
      * Oekaki-Pix
      * Mosaic
      * Mosaik
      * MozaÃ¯ek
      * ArtMosaico
      * Count and Darken
      * Nampre puzzle
      * Komsu Karala!
      * Cuenta Y Sombrea
      * Mosaico
      * Voisimage
      * Magipic
      * Fill-In


  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  """
  Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated 
  picture hidden inside. Using logic alone, the solver determines which 
  squares are painted and which should remain empty until the hidden picture 
  is completely exposed.
  """
  
  Fill-a-pix History:
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/fill_a_pix.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/fill_a_pix.pl
  * ECLiPSe: http://www.hakank.org/eclipse/fill_a_pix.ecl

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

namespace {

  // List of problems
  extern const int* problems[];

  // Number of specifications
  extern const unsigned int n_examples;

}


class FillAPix : public Script {

protected:

  static const int X = -1;
  const int* prob;  // problem instance

  int n; // size of the grid

  IntVarArray x;   // the grid

  // size of the problem instance
  int prob_size(void) const {
    return prob[0];
  }

public:

  FillAPix(const SizeOptions& opt) 
    : 
    prob(problems[opt.size()]),
    x(*this, prob_size()*prob_size(), 0, 1)
  {

    n = prob_size();
    cout << "problem : " << opt.size() << " size: " << n << endl;

    int c = 1; // index for access to data prob

    for(int i = 0; i < n; i++) {      
      for(int j = 0; j < n; j++) {
        int p = prob[c++]; // get data
        if (p > X) {
          IntVarArgs tmp;
          for(int a = -1; a <= 1; a++) {
            for(int b = -1; b <= 1; b++) {
              if (i+a >= 0 && i+a < n &&
                  j+b >= 0 && j+b < n) {
                tmp << x[(i+a)*n+(j+b)];
              }
            }
          }
          rel(*this, p == sum(tmp));
        }
      }
    }

    // branching
    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());
 
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << endl;
    for(int i = 0; i < n; i++) {
      os << "  ";
      for(int j = 0; j < n; j++) {
        int v = x[i*n+j].val();
        if (v == 1) {
          os << "X";
        } else {
          os << ".";
        } 
      }
      os << endl;
    }
    os << endl;

  }


  // Constructor for cloning s
  FillAPix(bool share, FillAPix& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new FillAPix(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("FillAPix");

  opt.solutions(0);
  opt.size(0);
  opt.parse(argc,argv);

  if (opt.size() >= n_examples) {
    cout << "Problems are between << 0.. " << n_examples-1 << endl;
    return -1;
  }

  Script::run<FillAPix,DFS,SizeOptions>(opt);    

  return 0;
}



namespace {

  /** Problem specifications
   *
   *  We code the problem instances with the following:
   *
   *   n,
   *   the matrix n x n
   * 
   */

  int X = -1;

  // Example 1
  // Puzzle 1 from 
  // http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  const int p1[] = {
    10,
    X,X,X,X,X,X,X,X,0,X,
    X,8,8,X,2,X,0,X,X,X,
    5,X,8,X,X,X,X,X,X,X,
    X,X,X,X,X,2,X,X,X,2,
    1,X,X,X,4,5,6,X,X,X,
    X,0,X,X,X,7,9,X,X,6,
    X,X,X,6,X,X,9,X,X,6,
    X,X,6,6,8,7,8,7,X,5,
    X,4,X,6,6,6,X,6,X,4,
    X,X,X,X,X,X,3,X,X,X
    };

  // Example 2
  // Puzzle 2 from 
  //  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  const int p2[] = {
    10,
    0,X,X,X,X,X,3,4,X,3,
    X,X,X,4,X,X,X,7,X,X,
    X,X,5,X,2,2,X,4,X,3,
    4,X,6,6,X,2,X,X,X,X,
    X,X,X,X,3,3,X,X,3,X,
    X,X,8,X,X,4,X,X,X,X,
    X,9,X,7,X,X,X,X,5,X,
    X,X,X,7,5,X,X,3,3,0,
    X,X,X,X,X,X,X,X,X,X,
    4,4,X,X,2,3,3,4,3,X
  };

  // Example 3
  // Puzzle from 
  // http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
  //
  // Code: 030.15x15
  // ID: 03090000000
  // 
  const int p3[] = {
    15,
    X,5,X,6,X,X,X,X,X,X,6,X,X,X,X,
    X,X,7,6,X,4,X,X,4,X,X,8,9,X,5,
    5,X,X,5,X,5,X,3,X,6,X,7,X,X,6,
    4,X,2,X,4,X,4,X,3,X,2,X,X,9,X,
    X,X,X,5,X,4,X,3,X,4,X,4,5,X,6,
    X,4,3,3,4,X,X,X,4,X,2,X,X,X,X,
    X,X,X,X,X,X,X,X,X,5,X,X,X,4,X,
    3,X,3,X,X,3,X,X,X,5,X,4,4,X,X,
    X,X,X,4,3,X,3,3,X,X,5,7,6,X,X,
    4,X,X,X,2,X,3,3,2,X,8,9,X,5,X,
    X,X,3,X,X,X,X,5,X,X,7,X,8,X,X,
    4,X,X,3,2,X,X,X,X,X,7,X,X,6,X,
    X,X,4,X,5,4,4,X,X,9,6,X,X,X,X,
    X,3,5,7,X,6,X,X,X,X,X,X,7,X,X,
    X,X,4,6,6,X,X,X,6,5,X,X,X,4,X
    };


  const int *problems[] = {p1, p2, p3};
  const unsigned n_examples = sizeof(problems)/sizeof(int*);

}
