/*

  Clock triplet puzzle in Gecode.
 
  Problem formulation
  http://www.f1compiler.com/samples/Dean%20Clark%27s%20Problem.f1.html
  """
  Dean Clark's Problem (Clock Triplets Problem)

  The problem was originally posed by Dean Clark and then presented
  to a larger audience by Martin Gardner. 

  The problem was discussed in Dr. Dobbs's Journal, May 2004 in an article 
  by Timothy Rolfe. According to the article, in his August 1986 column for 
  Isaac Asimov's Science Fiction Magazine, Martin Gardner presented this problem:
  
    Now for a curious little combinatorial puzzle involving the twelve
    numbers on the face of a clock. Can you rearrange the numbers (keeping
    them in a circle) so no triplet of adjacent numbers has a sum higher 
    than 21? This is the smallest value that the highest sum of a triplet
    can have.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/clock_triplets.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/clock_triplets.pl
  * ECLiPSe: http://www.hakank.org/eclipse/clock_triplets.ecl

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

class ClockTriplets : public Script {
protected:

  static const int n = 12;

  // the clock
  IntVarArray x;
  IntVar triplet_sum;

public:


  ClockTriplets(const Options& opt) 
    : 
    x(*this, n, 1, n),
    triplet_sum(*this, 0, 21)
  {

    // rel(*this, triplet_sum <= 21);
    distinct(*this, x);

    // symmetry breaking
    rel(*this, x[0] == 12);
    rel(*this, x[1] > x[11]);

    for(int i = 2; i <= 11; i++) {
      rel(*this, x[i] + x[i-1] + x[i-2] <= triplet_sum);
    }
    // around the corner
    rel(*this, x[10] + x[11] + x[0] <= triplet_sum);
    rel(*this, x[11] + x[0] + x[1] <= triplet_sum);

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
 
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {

    os << x << endl;
    os << "       " << x[0] << endl;
    os << "     " << x[11] << "    " << x[1] << endl;
    os << "   " << x[10] << "       " << x[2] << endl;
    os << "  " << x[9] << "         " << x[3] << endl;
    os << "   " << x[8] << "        " << x[4] << endl;
    os << "     " <<  x[7] << "    " << x[5] << endl;
    os << "       " << x[6] << endl;
    os << endl;
  }


  // Constructor for cloning s
  ClockTriplets(bool share, ClockTriplets& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ClockTriplets(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("ClockTriplets");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<ClockTriplets,DFS,Options>(opt);    

  return 0;
}
