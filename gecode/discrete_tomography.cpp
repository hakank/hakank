/*

  Discrete tomography in Gecode.


  Problem from http://eclipse-clp.org/examples/tomo.ecl.txt
  """
  This is a little "tomography" problem, taken from an old issue
  of Scientific American.
 
  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:
 
 ?- go.
     0 0 7 1 6 3 4 5 2 7 0 0
  0                         
  0                         
  8      * * * * * * * *    
  2      *             *    
  6      *   * * * *   *    
  4      *   *     *   *    
  5      *   *   * *   *    
  3      *   *         *    
  7      *   * * * * * *    
  0                         
  0                         
 
 
  Eclipse solution by Joachim Schimpf, IC-Parc
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/tomography.mzn
  * Comet   : http://www.hakank.org/discrete_tomography.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

namespace {

  // List of problems
  extern const int* problems[];
  // Number of specifications
  extern const unsigned int n_examples;

}



class DiscreteTomography : public Script {
protected:

  // Problem to be used
  const int* prob;

  IntVarArray x;     // Fields of matrix

  // Return rows of matrix
  int rows(void) const {
    return prob[0];
  }

  // Return columns of matrix
  int cols(void) const {
    return prob[1];
  }


public:

  DiscreteTomography(const SizeOptions& opt) 
    : prob(problems[opt.size()]),
      x(*this, rows()*cols(), 0, 1)
  {

    Matrix<IntVarArray> m(x, cols(), rows());

    // the rows
    int s = 2; // counter for the rows/cols sums (in prob)
    for(int i = 0; i < rows(); i++) {
      linear(*this, m.row(i), IRT_EQ, prob[s++], opt.icl());
    }
    
    // the columns
    for(int j = 0; j < cols(); j++) {
      linear(*this, m.col(j), IRT_EQ, prob[s++], opt.icl());
    }
    

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    Matrix<IntVarArray> m(x, cols(), rows());
    for(int i = 0; i < rows(); i++) {
      for(int j = 0; j < cols(); j++) {
        os << m(j,i) << " ";
      }
      os << std::endl;
    }
    os << std::endl;
  }

  // Constructor for cloning s
  DiscreteTomography(bool share, DiscreteTomography& s) : Script(share,s), prob(s.prob) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new DiscreteTomography(share,*this);
  }

};


int
main(int argc, char* argv[]) {

    SizeOptions opt("DiscreteTomography");
    opt.solutions(0);

    opt.parse(argc,argv);
    if (!opt.size()) 
      opt.size(0);

    if (opt.size() > n_examples-1) {
      std::cerr << "example must be between 0 and " << n_examples-1 << std::endl;
      return 1;
    }
    
    Script::run<DiscreteTomography,DFS,SizeOptions>(opt);

    return 0;
}


namespace {

  /** Problem specifications
   *
   * A specification is given by a list of integers. The first two
   * integers (r and c) specify the number of rows and column
   * respectively. Then r + c groups of integers follows with 
   * row sums and column sums.
   */

  //
  // Problem 1 .. 3 is from the ECLiPSe program cited above.
  //

  // Specification for problem 1 (see above)
  const int p1[] =
    { 11, 12,
      // Rows sums
      0,0,8,2,6,4,5,3,7,0,0,
      // Column sums
      0,0,7,1,6,3,4,5,2,7,0,0
    };
  
  // Specification for problem 2
  const int p2[] =
  { 5, 13,
    // Row sums
    10,4,8,5,6,
    // Columns sums
    5,3,4,0,5,0,5,2,2,0,1,5,1
  };
  
  // Specification for problem 3
  const int p3[] =
    { 3, 11,
      // Row sums
      11,5,4,
      // Column sums
      3,2,3,1,1,1,1,2,3,2,1
    };

  // Specification for problem 4
  // This is my own (hakank's) creation
  const int p4[] =
    { 14, 14,
      // Row sums
      0,2,2, 2, 2,2,8,8,4,4,4,4,4,0,
      // Column sums
      0,0,0,12,12,2,2,2,2,7,7,0,0,0
    };
  

  const int *problems[] = {p1, p2, p3, p4};
  const unsigned n_examples = sizeof(problems)/sizeof(int*);
  
}


