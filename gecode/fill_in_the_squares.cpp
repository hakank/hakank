/*

  Fill in the squares problem (Brainjammer) in Gecode.

 This problem is from the ZDC system, available from 
  http://www.bracil.net/CSP/cacp/cacpdemo.html , in the
  file 
     Brainjammer.txt 
  from 2003-01-26, which states:
  """
  Only Solution is:
        1	2	3	4	5
  ====================================================
  A  	 7	11	2	17	1
  B	13	19	23	22	3
  C	9	20	24	14	12
  D	16	21	25	18	10
  E	4	8	15	6	5

  22mins55secs of CPU time to find first solution
  50mins42secs of CPU time with duplicate induced variables removed?
  Maybe this has something to do with the variable ordering...as this might change
  as a result of removing duplicate induced variables.
  1hr:34 mins of CPU time to find a single solution and determine no other solutions
  exist.

  Statistics for finding the first solution:
  (with duplicate induced nodes removed)
  CPU seconds: 		4880.63	(On a Pentium Pro 200Mhz, VC++)
  Node count:			4036162
  Induced node count:	1849214
  Backtracks:			5885311

  """

  The only references to this problem I've found is the following pages:
    http://discuss.fogcreek.com/techInterview/default.asp?cmd=show&ixPost=2787
    http://notdarkandstormy.blogspot.com/2005/05/funky-logic-problem.html
  and especially 
    http://perplexus.info/show.php?pid=2683
  which has a lot of comments about manually solving the problem.

  I've yet to know the original source.


  Compare with the following models:
  * Comet: http://hakank.org/comet/fill_in_the_squares.co
  * MiniZinc: http://hakank.org/minizinc/fill_in_the_squares.mzn

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

// is a square?
void is_square(Space& space, IntVar x) {
  IntVar y(space, 0, 100);
  rel(space, y*y == x);
}

// is a prime?
void is_prime(Space& space, IntVar x) {
  for(int i = 2; i <= 99; i++) {
    rel(space, (i < x) >> (x % i > 0));
  }
}


class FillInTheSquares : public Script {
protected:

  static const int n = 5;

  IntVarArray a; 
  IntVarArray b; 
  IntVarArray c; 
  IntVarArray d;
  IntVarArray e;

  IntVarArray ALL; // all numbers

public:


  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  FillInTheSquares(const Options& opt) 
  : 
    a(*this, n, 1, n*n), 
    b(*this, n, 1, n*n), 
    c(*this, n, 1, n*n), 
    d(*this, n, 1, n*n), 
    e(*this, n, 1, n*n), 
    ALL(*this, n*n, 1, n*n)
    
  {

    int max_sum = 0;
    for(int i = 21; i <= 25; i++) {
      max_sum += i;
    }
    IntVar a_sum(*this, 1, max_sum);
    IntVar b_sum(*this, 1, max_sum);
    IntVar c_sum(*this, 1, max_sum);
    IntVar d_sum(*this, 1, max_sum);
    IntVar e_sum(*this, 1, max_sum);

    rel(*this, a_sum == sum(a));
    rel(*this, b_sum == sum(b));
    rel(*this, c_sum == sum(c));
    rel(*this, d_sum == sum(d));
    rel(*this, e_sum == sum(e));

    // Each number from 1-25, used only once
    for(int i = 0; i < n; i++) {
      rel(*this, ALL[i]     == a[i]);
      rel(*this, ALL[i+n]   == b[i]);
      rel(*this, ALL[i+2*n] == c[i]);
      rel(*this, ALL[i+3*n] == d[i]);
      rel(*this, ALL[i+4*n] == e[i]);
    }
    distinct(*this, ALL);

    // 1.Sum of each column is odd
    for(int i = 0; i < n; i++) {
      rel(*this, (a[i] + b[i] + c[i] + d[i] + e[i]) % 2 == 1);
    }

    // 2.Sum of each row, except C is even
    rel(*this, a_sum % 2 == 0) ;
    rel(*this, b_sum % 2 == 0) ;
    rel(*this, c_sum % 2 == 1) ;
    rel(*this, d_sum % 2 == 0) ;
    rel(*this, e_sum % 2 == 0) ;
    
    
    // 3.Sum of row A is not greater than the sum of any other row
    rel(*this, a_sum <= b_sum);	
    rel(*this, a_sum <= c_sum);	
    rel(*this, a_sum <= d_sum);	
    rel(*this, a_sum <= e_sum);	
    
  
    // 4.The sum of diagonal A1 to E5 is greater than the sum of
    //   diagonal E1 to A5
    rel(*this, a[0] + b[1] + c[2] + d[3] + e[4]  > e[0] + d[1] + c[2] + b[3] + a[4]);
    
    // 5.(A4 + B4) is greater than (C4+D4+E4)
    rel(*this, a[3] + b[3] > c[3] + d[3] + e[3]);
    
    // 6. A1 + B1 = D1 + E1
    rel(*this, a[0] + b[0] == d[0] + e[0]); 
    
    // 7. A1 > E1
    rel(*this, a[0] > e[0]); 
    
    // 8. A1, A3 and B1 are primes
    is_prime(*this, a[0]);
    is_prime(*this, a[2]);
    is_prime(*this, b[0]);
  
    // 9.(A3 + E3) is a prime number
    is_prime(*this, expr(*this, a[2]+e[2]));

  
    // 10. A5,D1,D3 and E1 are squares
    is_square(*this, a[4]);
    is_square(*this, d[0]); 
    is_square(*this, d[2]); 
    is_square(*this, e[0]); 
    
    // 11. B2, C2, and D2 are ascending consecutive numbers
    rel(*this, b[1] + 1 == c[1]); 
    rel(*this, c[1] + 1 == d[1]); 
    
    // 12. B3, C3, and D3 are ascending consecutive numbers
    rel(*this, b[2] + 1 == c[2]); 
    rel(*this, c[2] + 1 == d[2]); 
    
    // 13. B5 + D5 = A5 + C5
    rel(*this, b[4] + d[4] == a[4] + c[4]); 
  
    // 14. (c1)^2 + (c5)^2 = (e3)^2
    rel(*this, c[0]*c[0] + c[4]*c[4] == e[2]*e[2]);
    
    // 15. C5 is a two-digit number
    rel(*this, c[4] > 9); 
    
    // 16. D5 is a multiple of E5
    rel(*this, d[4] % e[4] == 0); 
    
    // 17. E1 + E3 = E2 + E4 + E5
    rel(*this, e[0] + e[2] == e[1] + e[3] + e[4]);
    

    branch(*this, ALL, INT_VAR_SIZE_MIN(), INT_VAL_MAX()); 
    branch(*this, a, INT_VAR_SIZE_MIN(), INT_VAL_MAX()); 
    branch(*this, b, INT_VAR_SIZE_MIN(), INT_VAL_MAX()); 
    branch(*this, c, INT_VAR_SIZE_MIN(), INT_VAL_MAX()); 
    branch(*this, d, INT_VAR_SIZE_MIN(), INT_VAL_MAX()); 
    branch(*this, e, INT_VAR_SIZE_MIN(), INT_VAL_MAX()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "a: " << a << endl;
    os << "b: " << b << endl;
    os << "c: " << c << endl;
    os << "d: " << d << endl;
    os << "e: " << e << endl;
    os << endl;

  }

  // Constructor for cloning s
  FillInTheSquares(bool share, FillInTheSquares& s) : Script(share, s) {
    a.update(*this, share, s.a);
    b.update(*this, share, s.b);
    c.update(*this, share, s.c);
    d.update(*this, share, s.d);
    e.update(*this, share, s.e);
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new FillInTheSquares(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("FillInTheSquares");

  opt.solutions(0);
  opt.icl(ICL_BND);
  opt.parse(argc,argv);
  Script::run<FillInTheSquares,DFS,Options>(opt); 

  return 0;

}


