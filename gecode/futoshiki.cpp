/*

  Futoshiki puzzle in Gecode.
  
  http://en.wikipedia.org/wiki/Futoshiki
  """
  The puzzle is played on a square grid, such as 5 x 5. The objective
  is to place the numbers 1 to 5 (or whatever the dimensions are) such 
  that each row, and column contains each of the digits 1 to 5. Some 
  digits may be given at the start. In addition, inequality
  constraints are also initially specifed between some of the squares, 
  such that one must be higher or lower than its neighbour. These 
  constraints must be honoured as the grid is filled out.
  """

  See also this Guardian article from 2006
  http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley
 
  This was inspired from the Minion/Tailor example futoshiki.eprime .

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/futoshiki.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/futoshiki.pl
  * ECLiPSe: http://www.hakank.org/eclipse/futoshiki.ecl


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


class Futoshiki : public Script {
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
  Futoshiki(const SizeOptions& opt) : 
    prob(problems[opt.size()]),
    x(*this, prob_size()*prob_size(), 1, prob_size())
  {

    int n = prob_size();
    std::cout << "Problem " << opt.size() << " size: " << n << std::endl;

    //
    // get data for this problem
    //
    int s = 1; // we have already taken n (the size)

    // Get the grid hints
    int values[n*n];
    std::cout << "values: " << std::endl;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        values[i*n+j] = prob[s++];
        if (values[i*n+j] > 0) {
          std::cout << values[i*n+j] << " ";
        } else {
          std::cout << "." << " ";
        }
        // set the hints
        if (values[i*n+j] > 0) {
          rel(*this,x[i*n+j] == values[i*n+j], opt.icl());
        }
      }
      std::cout << std::endl;
    }
    
    // Get the lt (less than) hints
    int num_lt = prob[s++];
    int lt[num_lt*4];
    std::cout << "\nlt hints: " << std::endl;
    for(int i = 0; i < num_lt; i++) {
      for(int j = 0; j < 4; j++) {
        lt[i*4+j] = prob[s++];
        std::cout << lt[i*4+j] << " " ;
        if (j == 1) {
          std::cout << "< " ;
        }
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
    // Lt hints
    // Note: the hints refers to a 1-based grid so we
    //       convert to 0-based
    for(int i = 0; i < num_lt ; i++) {
      int lt0 = lt[i*4+0]-1;
      int lt1 = lt[i*4+1]-1;
      int lt2 = lt[i*4+2]-1;
      int lt3 = lt[i*4+3]-1;
      rel(*this, x_m(lt1,lt0) < x_m(lt3,lt2), opt.icl()); // matrix access
      // rel(*this, x[lt0*n+lt1] < x[lt2*n+lt3], opt.icl()); // direct on x
    }

    branch(*this, x, INT_VAR_REGRET_MIN_MAX(), INT_VAL_MIN());

  }

  // Constructor for cloning s
  Futoshiki(bool share, Futoshiki& s) : Script(share,s), prob(s.prob) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Futoshiki(share,*this);
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
  SizeOptions opt("Futoshiki");
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


  Script::run<Futoshiki,DFS,SizeOptions>(opt);

  return 0;
}


//
// The problems
//
// Note: The indices for placed 1-based matrix
// It is handled by the model.
namespace {

  // Problem 1
  // Example from Tailor model futoshiki.param/futoshiki.param
  const int p1[] = 
    { 
      // size
      5,
      // values, hints in the grid
      0, 0, 3, 2, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      // number of lt hints
      11,
      // lt entries
      1,2,1,1,
      1,4,1,5,
      2,3,1,3,
      3,3,2,3,
      3,4,2,4,
      2,5,3,5,
      3,2,4,2,
      4,4,4,3,
      5,2,5,1,
      5,4,5,3,
      5,5,4,5
    };

  // Problem 2
  // Example from http://en.wikipedia.org/wiki/Futoshiki
  const int p2[] = 
    {
      // size
      5,
      // values, hints in the grid
      0, 0, 0, 0, 0,
      4, 0, 0, 0, 2,
      0, 0, 4, 0, 0,
      0, 0, 0, 0, 4,
      0, 0, 0, 0, 0,
      // number of lt hints
      6,
      // lt hints
      1,2, 1,1,
      1,4, 1,3,
      1,5, 1,4,
      4,4, 4,5,
      5,1, 5,2,
      5,2, 5,3
    };


  // Problem 3
  // From http://www.brainbashers.com/showfutoshiki.asp
  // 12 June 2010, Hard
  const int p3[] = 
    {
      // size
      7,
      // values, hints in the grid
      0,2,7,3,0,0,5,
      0,0,3,0,0,0,0,
      0,0,0,2,0,0,0,
      0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      0,6,0,0,0,0,0,
      0,0,0,0,0,0,6,
      // number of lt hints
      17,
      // lt hints
      1,2, 1,1,
      2,5, 1,5,
      3,2, 4,2,
      3,6, 3,7,
      4,1, 4,2,
      4,2, 5,2,
      4,3, 4,2,
      4,3, 5,3,
      4,4, 5,4,
      5,1, 5,2,
      5,2, 6,2,
      5,4, 6,4,
      5,5, 6,5,
      5,6, 4,6,
      6,1, 7,1,
      6,3, 7,3,
      7,6, 7,7      
    };

  const int *problems[] = {p1,p2,p3};
  const unsigned n_examples = sizeof(problems)/sizeof(int*);


}
