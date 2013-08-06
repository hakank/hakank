/*

  Minesweeper problem in Gecode.
 
  This Minesweeper model is a port from my MiniZinc model
  http://www.hakank.org/minzinc/minesweeper.mzn

  Some of the examples was borrowed from the Minesweeper model in
  the Gecode distribution:
  http://www.gecode.org/doc-latest/reference/minesweeper_8cpp_source.html
  """
  A specification is a square matrix of characters. Alphanumeric characters 
  represent the number of mines adjacent to that field. Dots represent 
  fields with an unknown number of mines adjacent to it (or an actual mine).
  """
  E.g.
       "..2.3."
       "2....."
       "..24.3"
       "1.34.."
       ".....3"
       ".3.3.."
  """

  Also see the following pages:   
  * http://www.janko.at/Raetsel/Minesweeper/index.htm
  * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
    Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/
  * Richard Kaye's Minesweeper Pages
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
  * Some Minesweeper Configurations
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf

  Compare with the Fill-a-pix model:
  http://www.hakank.org/gecode/fill_a_pix.cpp

  Also, compare with the implementations in other systems:
  * MiniZinc: http://www.hakank.org/minizinc/minesweeper.mzn  
  * Comet: http://www.hakank.org/comet/minesweeper.co
  * Choco: http://www.hakank.org/choco/MineSweeper.java
  * ECLiPSe: http://www.hakank.org/eclipse/minesweeper.ecl
  * Tailor/Essence': http://www.hakank.org/tailor/minesweeper.eprime
  * Gecode/R: http://www.hakank.org/gecode_r/minesweeper.rb
  * JaCoP: http://www.hakank.org/JaCoP/MineSweeper.java
  * SICStus Prolog: http://www.hakank.org/sicstus/minesweeper.pl



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


class Minesweeper : public Script {

protected:

  const int* prob;  // problem instance

  // size of the grid rows X cols
  int rows;
  int cols;

  IntVarArray x;   // the grid

  // size of the problem instance
  int row_size(void) const {
    return prob[0];
  }

  int col_size(void) const {
    return prob[1];
  }


public:

  Minesweeper(const SizeOptions& opt) 
    : 
    prob(problems[opt.size()]),
    x(*this, row_size()*col_size(), 0, 1)
  {

    rows = row_size();
    cols = col_size();
    cout << "Problem " << opt.size() << " size: " << rows << " X " << cols << endl;

    int X = -1; // the unknowns
    // As an IntVar constant 
    // to be used in the constraints below
    IntVar XX(*this, X,X);

    int c = 2; // index for access to data prob


    for(int i = 0; i < rows; i++) {      
      cout << "  ";
      for(int j = 0; j < cols; j++) {
        int p = prob[c++]; // get data

        // some extra constraints
        IntVar pp(*this, p, p);
        rel(*this, (pp > XX) >> (x[i*cols+j] == 0));
        // rel(*this, (x[i*cols+j] == 1) >> (pp == XX));

        if (p == X) {
          cout << "X ";
        } else {
          cout << p << " ";
        }

        if (p > X) {
          IntVarArgs tmp;
          for(int a = -1; a <= 1; a++) {
            for(int b = -1; b <= 1; b++) {
              if (i+a >= 0 && i+a < rows &&
                  j+b >= 0 && j+b < cols) {
                tmp << x[(i+a)*cols+(j+b)];
              }
            }
          }
          rel(*this, p == sum(tmp));
        }
      }
      cout << endl;
    }

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
 
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    
    os << "\n  Solution: " << endl;
    for(int i = 0; i < rows; i++) {
      os << "  ";
      for(int j = 0; j < cols; j++) {
        os << x[i*cols+j] << " ";
      }
      os << endl;
    }
    os << endl;

  }


  // Constructor for cloning s
  Minesweeper(bool share, Minesweeper& s) : Script(share,s), rows(s.rows), cols(s.cols) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Minesweeper(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("Minesweeper");

  opt.solutions(0);
  opt.size(0);
  opt.parse(argc,argv);

  if (opt.size() >= n_examples) {
    cout << "Problems are between << 0.. " << n_examples-1 << endl;
    return -1;
  }

  Script::run<Minesweeper,DFS,SizeOptions>(opt);    

  return 0;
}



namespace {

  /** Problem specifications
   *
   *  We code the problem instances with the following:
   *
   *   n rows, n cols
   *   the matrix rows X cols
   * 
   */

  int X = -1;

  // The first 10 examples (0..9) are from
  // Gecode's Minesweeper model:
  // http://www.gecode.org/doc-latest/reference/minesweeper_8cpp_source.html
  const int p0[] = {
    // 0
    6,6,
    X,X,2,X,3,X,
    2,X,X,X,X,X,
    X,X,2,4,X,3,
    1,X,3,4,X,X,
    X,X,X,X,X,3,
    X,3,X,3,X,X
  };

  const int p1[] = {
    // 1
    8,8,
    X,2,X,2,1,1,X,X,
    X,X,4,X,2,X,X,2,
    2,X,X,2,X,X,3,X,
    2,X,2,2,X,3,X,3,
    X,X,1,X,X,X,4,X,
    1,X,X,X,2,X,X,3,
    X,2,X,2,2,X,3,X,
    1,X,1,X,X,1,X,1
  };

  const int p2[] = {
    // 2
    10,10,
    1,X,X,2,X,2,X,2,X,X,
    X,3,2,X,X,X,4,X,X,1,
    X,X,X,1,3,X,X,X,4,X,
    3,X,1,X,X,X,3,X,X,X,
    X,2,1,X,1,X,X,3,X,2,
    X,3,X,2,X,X,2,X,1,X,
    2,X,X,3,2,X,X,2,X,X,
    X,3,X,X,X,3,2,X,X,3,
    X,X,3,X,3,3,X,X,X,X,
    X,2,X,2,X,X,X,2,2,X
  };

  const int p3[] = {
    // 3
    8,8,
    2,X,X,X,3,X,1,X,
    X,5,X,4,X,X,X,1,
    X,X,5,X,X,4,X,X,
    2,X,X,X,4,X,5,X,
    X,2,X,4,X,X,X,2,
    X,X,5,X,X,4,X,X,
    2,X,X,X,5,X,4,X,
    X,3,X,3,X,X,X,2
  };

  const int p4[] = {
    // 4
    10,10,
    0,X,0,X,1,X,X,1,1,X,
    1,X,2,X,2,X,2,2,X,X,
    X,X,X,X,X,X,2,X,X,2,
    X,2,3,X,1,1,X,X,X,X,
    0,X,X,X,X,X,X,2,X,1,
    X,X,X,2,2,X,1,X,X,X,
    X,X,X,X,X,3,X,3,2,X,
    X,5,X,2,X,X,X,3,X,1,
    X,3,X,1,X,X,3,X,X,X,
    X,2,X,X,X,1,2,X,X,0
  };

  const int p5[] = {
    // 5
    10,10,
    X,2,1,X,2,X,2,X,X,X,
    X,4,X,X,3,X,X,X,5,3,
    X,X,X,4,X,4,4,X,X,3,
    4,X,4,X,X,5,X,6,X,X,
    X,X,4,5,X,X,X,X,5,4,
    3,4,X,X,X,X,5,5,X,X,
    X,X,4,X,4,X,X,5,X,5,
    2,X,X,3,3,X,6,X,X,X,
    3,6,X,X,X,3,X,X,4,X,
    X,X,X,4,X,2,X,2,1,X
  };

  const int p6[] = {
    // 6
    8,8,
    X,3,2,X,X,1,X,X,
    X,X,X,X,1,X,X,3,
    3,X,X,2,X,X,X,4,
    X,5,X,X,X,5,X,X,
    X,X,6,X,X,X,5,X,
    3,X,X,X,5,X,X,4,
    2,X,X,5,X,X,X,X,
    X,X,2,X,X,3,4,X
  };

  const int p7[] = {
    // 7
    9,9,
    X,1,X,X,X,X,X,3,X,
    X,X,X,3,4,3,X,X,X,
    2,4,4,X,X,X,4,4,3,
    X,X,X,4,X,4,X,X,X,
    X,4,X,4,X,3,X,6,X,
    X,X,X,4,X,3,X,X,X,
    1,2,3,X,X,X,1,3,3,
    X,X,X,3,2,2,X,X,X,
    X,2,X,X,X,X,X,3,X
  };

  const int p8[] = {
    // 8
    7,7,
    X,X,X,X,X,X,X,
    X,2,3,4,3,5,X,
    X,1,X,X,X,3,X,
    X,X,X,5,X,X,X,
    X,1,X,X,X,3,X,
    X,1,2,2,3,4,X,
    X,X,X,X,X,X,X
  };

  const int p9[] = {
    // 9
    9,9,
    2,X,X,X,2,X,X,X,2,
    X,4,X,4,X,3,X,4,X,
    X,X,4,X,X,X,1,X,X,
    X,4,X,3,X,3,X,4,X,
    2,X,X,X,X,X,X,X,2,
    X,5,X,4,X,5,X,4,X,
    X,X,3,X,X,X,3,X,X,
    X,4,X,3,X,5,X,6,X,
    2,X,X,X,1,X,X,X,2
  };


  /* 
     The following instances are taken from my 
     problems in MiniZinc. See http://www.hakank.org/minizinc/
  */

  
  // http://www.hakank.org/minizinc/minesweeper_basic3.mzn
  const int p10[] = {
    2,3,
    X,X,X,
    1,1,1   
  };

  // http://www.hakank.org/minizinc/minesweeper_basic4.mzn
  const int p11[] = {
    2,4,
    X,X,X,X,
    1,1,1,1 
  };

  // http://www.hakank.org/minizinc/minesweeper_basic4x4.mzn
  const int p12[] = {
    4,4,
    0,0,0,X,
    1,X,X,X,
    1,X,X,X,
    1,1,1,0
  };


  // http://www.hakank.org/minizinc/minesweeper_config_page2.mzn
  // From "Some Minesweeper Configurations",page 2
  const int p13[] = {
    6,6,
    X,X,X,X,X,X,
    X,2,2,2,2,X,
    X,2,0,0,2,X,
    X,2,0,0,2,X,
    X,2,2,2,2,X,
    X,X,X,X,X,X,
  };

  // http://www.hakank.org/minizinc/minesweeper_config_page3.mzn
  //  From "Some Minesweeper Configurations",page 3
  // 4 solutions
  const int p14[] = {
    8,8,
    2,3,X,2,2,X,2,1,
    X,X,4,X,X,4,X,2,
    X,X,X,X,X,X,4,X,
    X,5,X,6,X,X,X,2,
    2,X,X,X,5,5,X,2,
    1,3,4,X,X,X,4,X,
    0,1,X,4,X,X,X,3,
    0,1,2,X,2,3,X,2 
  };


  // http://www.hakank.org/minizinc/minesweeper_german_Lakshtanov.mzn
  // Oleg German, Evgeny Lakshtanov: "Minesweeper" without a computer
  // http://arxiv.org/abs/0806.3480, page 4
  const int p15[] = {
    5,6,
    X,1,X,1,X,1,
    2,X,2,X,1,X,
    X,3,X,2,X,1,
    1,X,3,X,2,X,
    X,1,X,2,X,1,
  };

  // http://www.hakank.org/minizinc/minesweeper_splitter.mzn
  // Richard Kaye: How Complicated is Minesweeper?
  // http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
  // A splitter,page 35
  // 131072 solutions.
  const int p16[] = {
    11,11,
    X,X,X,0,X,X,X,0,X,X,X,
    X,X,X,0,1,X,1,0,X,X,X,
    X,X,X,0,1,X,1,0,X,X,X,
    0,0,0,0,1,1,1,0,0,0,0,
    X,1,1,1,1,X,1,1,1,1,X,
    X,X,X,1,X,2,X,1,X,X,X,
    X,1,1,1,1,X,1,1,1,1,X,
    0,0,0,0,1,1,1,0,0,0,0,
    X,X,X,0,1,X,1,0,X,X,X,
    X,X,X,0,1,X,1,0,X,X,X,
    X,X,X,0,X,X,X,0,X,X,X,
  };

  // http://www.hakank.org/minizinc/minesweeper_wire.mzn
  // Richard Kaye: How Complicated is Minesweeper?
  // http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
  // A Wire,page 33
  // (2 solutions)
  const int p17[] = {
    5,14,
    X,0,0,0,0,0,0,0,0,0,0,0,0,X,
    X,1,1,1,1,1,1,1,1,1,1,1,1,X,
    X,X,1,X,X,1,X,X,1,X,X,1,X,X,
    X,1,1,1,1,1,1,1,1,1,1,1,1,X,
    X,0,0,0,0,0,0,0,0,0,0,0,0,X
  };

  const int *problems[] = 
    {
      p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
      p11,p12,p13,p14,p15,p16,p17};

  const unsigned n_examples = sizeof(problems)/sizeof(int*);


}
