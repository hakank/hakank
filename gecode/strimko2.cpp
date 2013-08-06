/*

  Strimko / Chain Sudoku puzzle in Gecode.
  
  From 
  360: A New Twist on Latin Squares
  http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
  """
  The idea is simple: each row and column of an nxn grid must contain 
  the number 1, 2, ... n exactly once (that is, the grid must form a 
  Latin square), and each "stream" (connected path in the grid) must 
  also contain the numbers 1, 2, ..., n exactly once.
  """

  For more information, see:
  * http://www.strimko.com/
  * http://www.strimko.com/rules.htm
  * http://www.strimko.com/about.htm
  * http://www.puzzlersparadise.com/Strimko.htm
  
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/strimko2.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/strimko2.pl
  * ECLiPSe: http://www.hakank.org/eclipse/strimko2.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

namespace {
  // List of problems
  extern const int* problems[];
  // Number of specifications
  extern const unsigned int n_examples;

}

class Strimko : public Script {
protected:

  // Problem to be used
  const int* prob;

  // solution
  IntVarArray x;

  // Return size of matrix
  int prob_size(void) const {
    return prob[0];
  }


public:

  // Actual model
  Strimko(const SizeOptions& opt) : 
    prob(problems[opt.size()]),
    x(*this, prob_size()*prob_size(), 1, prob_size())
  {

    int n = prob_size();
    std::cout << "Problem " << opt.size() << " size: " << n << std::endl;

    //
    // get data for this problem
    //
    int s = 1; // we have already taken n (the size)

    // streams
    int streams[n*n];
    std::cout << "streams: " << std::endl;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        streams[i*n+j] = prob[s++];
        std::cout << streams[i*n+j] << " ";        
      }
      std::cout << std::endl;
    }
    
    // placed
    int num_placed = prob[s++];// 5;
    int placed[num_placed*3];
    std::cout << "\nplaced: " << std::endl;
    for(int i = 0; i < num_placed; i++) {
      for(int j = 0; j < 3; j++) {
        placed[i*3+j] = prob[s++];
        std::cout << placed[i*3+j] << " " ;
      }
      std::cout << std::endl;
    }
    std::cout << std::endl;

    Matrix<IntVarArray> x_m(x, n, n);

    /*
     *
     * Constraints
     *
     */

    //
    // Latin Square
    //
    for (int i = 0; i < n; i++) {
      distinct(*this, x_m.row(i), opt.icl());
      distinct(*this, x_m.col(i), opt.icl());
    }

    //
    // Streams
    //
    for(int st = 1; st <= n; st++) {
      IntVarArgs stream;
      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          if (streams[i*n+j] == st) {
            // stream << x[i*n+j]; // direct on x
            stream << x_m(j,i); // matrix access
          }
        }
      }
      distinct(*this, stream, opt.icl());
    }

    //
    // Placed hints
    //
    for(int i = 0; i < num_placed; i++) {
      int pi  = placed[i*3]-1;
      int pi1 = placed[i*3+1]-1;
      int pi2 = placed[i*3+2];
      // rel(*this, x[pi*n+pi1] == pi2, opt.icl()); // direct on x
      rel(*this, x_m(pi1,pi) == pi2, opt.icl()); // matrix access
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Constructor for cloning s
  Strimko(bool share, Strimko& s) : Script(share,s), prob(s.prob) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Strimko(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    int n = prob_size();
    os << "Solution: " << std::endl;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        os << x[i*n+j] << " ";        
      }
      os << std::endl;
    }
    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("Strimko");
  opt.solutions(0);
  opt.icl(ICL_DOM);

  opt.parse(argc,argv);

  if (!opt.size()) {
    opt.size(0); // default problem
  }

  if (opt.size() > n_examples-1) {
    std::cerr << "example must be between 0 and " << n_examples-1 << std::endl;
    return 1;
  }


  Script::run<Strimko,DFS,SizeOptions>(opt);

  return 0;
}


//
// The problems
//
// Note: The indices for placed 1-based matrix
// It is handled by the model.
namespace {

  // Problem 1
  // Strimko Weekly Set 067
  const int p1[] = 
    { 
      // n
      5,
      // streams
      1,1,1,2,3,
      1,2,2,2,3,
      1,2,4,5,3,
      5,4,5,4,3,
      4,5,5,4,3,
      // num_placed
      5,
      // placed
      1,3,4,
      1,4,1,
      3,3,2,
      3,5,3,
      5,4,5
    };

  // Problem 2
  // Strimko Monthly #02
  const int p2[] = 
    {
      // n
      7,
      // streams
      1,1,2,2,2,2,2,
      1,1,2,3,3,3,2,
      1,4,1,3,3,5,5,
      4,4,3,1,3,5,5,
      4,6,6,6,7,7,5,
      6,4,6,4,5,5,7,
      6,6,4,7,7,7,7,
      // num_placed 
      10,
      // placed
      2,1,1,
      2,3,7,
      2,5,6,
      2,7,4,
      3,2,7,
      3,6,1,
      4,1,4,
      4,7,5,
      5,2,2,
      5,6,6
    };

  // Problem 3
  //  Strimko Weekly Set 068
  const int p3[] = 
    {
      // n
      4,
      // streams
      1,2,2,4,
      2,1,4,2,
      3,4,1,3,
      4,3,3,1,
      // num_placed
      3,
      // placed
      2,2,3,
      2,3,2,
      3,3,1
    };

  // Problem 4
  // Strimko Weekly Set 069
  const int p4[] = 
    {
      // n
      6,
      // streams
      1,2,3,3,3,4,
      2,1,3,5,4,3,
      2,1,3,5,5,4,
      2,6,1,6,5,4,
      2,6,1,6,4,5,
      6,2,6,1,5,4,
      // num_placed
      8,
      // placed
      2,2,4,
      2,3,1,
      2,4,3,
      2,5,2,
      3,2,1,
      3,5,6,
      4,3,5,
      4,4,2
    };

  // Problem 5
  // Strimko Weekly Set 070
  const int p5[] = 
    {
      // n
      5,
      // streams
      1,2,3,3,3,
      2,1,1,3,1,
      2,2,3,1,4,
      5,2,5,4,4,
      5,5,5,4,4,
      // num_placed
      4,
      // placed
      1,1,1,
      2,5,4,
      4,1,2,
      5,4,5
    };


  // Problem 6
  // Chain Sudoku / Strimko
  // From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/sudoku/chain
  // 
  //  Chain Sudoku 9x9
  // Difficulty: Very hard
  // Code: 069.9x9
  //  ID: 06990000006
  const int p6[] = 
    {
      // n
      9,
      // streams
      1,1,2,2,2,2,3,3,3,
      1,1,2,2,2,3,2,3,4,
      1,1,2,3,3,3,3,4,4,
      1,5,6,6,6,7,7,7,4,
      1,5,5,6,6,6,7,4,7,
      1,5,5,5,5,7,6,7,4,
      8,5,5,9,7,7,6,4,4,
      8,8,8,9,9,9,6,9,4,
      8,8,8,8,8,9,9,9,9,
      // num_placed
      21,
      // placed
      1,5,8,
      1,8,2,
      2,1,4,
      2,3,7,
      3,5,6,
      3,8,7,
      4,4,9,
      4,6,2,
      5,1,8,
      5,3,1,
      5,5,5,
      5,7,6,
      5,9,3,
      6,4,5,
      6,6,7,
      7,2,6,
      7,5,4,
      8,7,8,
      8,9,5,
      9,2,8,
      9,5,7
    };

  const int *problems[] = {p1, p2, p3, p4, p5, p6};
  const unsigned n_examples = sizeof(problems)/sizeof(int*);


}
