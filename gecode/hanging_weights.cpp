/*
  
  Hanging weights puzzle in Gecode.

  From 
  "Using LINQ to solve puzzles"
  http://blogs.msdn.com/lukeh/archive/2007/03/19/using-linq-to-solve-puzzles.aspx
  """
  Here's a puzzle similar to the one in the puzzle hunt.  The diagram 
  below is a bunch of weights (A-M) hanging from a system of bars.  
  Each weight has an integer value between 1 and 13, and the goal is 
  to figure out what each weight must be for the the diagram below to 
  balance correctly as shown: 

                           |
                           |
               +--+--+--+--+--+--+--+
               |                    |
               |                    |
            +--+--+--+--+--+        |
            |     L        M        |
            |                       |
   +--+--+--+--+--+--+     +--+--+--+--+--+
   H              |  I     |  J        K  |
                  |        |              |
         +--+--+--+--+--+  |     +--+--+--+--+--+
         E              F  |     G              |
                           |                    |
               +--+--+--+--+--+  +--+--+--+--+--+--+
               A              B  C                 D

  The rules for this kind of puzzle are: 
  (1) The weights on either side of a given pivot point must be equal, 
      when weighted by the distance from the pivot, and 
  (2) a bar hanging beneath another contributes it's total weight as 
      through it were a single weight.  For instance, the bar on the bottom 
      right must have 5*C=D, and the one above it must have 3*G=2*(C+D).
  """
  
 
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/hanging_weights.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/hanging_weights.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class HangingWeights : public Script {
protected:

  static const int n = 13; // length of the arrays

  IntVarArray x;
  IntVar total; 

public:

  HangingWeights(const Options& opt) 
    : 
    x(*this, n, 1, n),
    total(*this, 0, n*13)
  {

    IntVar
      a(x[0]),
      b(x[1]),
      c(x[2]),
      d(x[3]),
      e(x[4]),
      f(x[5]),
      g(x[6]),
      h(x[7]),
      i(x[8]),
      j(x[9]),
      k(x[10]),
      l(x[11]),
      m(x[12]);
    
    distinct(*this, x, opt.icl());

    // Not very beautiful, but experimental...
    rel(*this, (4 * a == b) &&
        (5 * c == d) &&
        (3 * e == 2 * f) &&
        (3 * g == 2 * (c + d)) &&
        (3 * (a + b) + 2 * j == k + 2 * (g + c + d)) &&
        (3 * h == 2 * (e + f) + 3 * i) &&
        ((h + i + e + f) == l + 4 * m) &&
        (4 * (l + m + h + i + e + f) == 3 * (j + k + g + a + b + c + d)) &&
        (total == sum(x)), 
        opt.icl()
        );

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());



  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;
    os << "total: " << total << endl;
    os << endl;

    IntVar
      a(x[0]),
      b(x[1]),
      c(x[2]),
      d(x[3]),
      e(x[4]),
      f(x[5]),
      g(x[6]),
      h(x[7]),
      i(x[8]),
      j(x[9]),
      k(x[10]),
      l(x[11]),
      m(x[12]);

    os << "                          |"                             << endl;
    os << "                          |" << endl;
    os << "              +--+--+--+--+--+--+--+" << endl;
    os << "              |                    |" << endl;
    os << "              |                    |"<< endl;
    os << "           +--+--+--+--+--+        |"<< endl;
    os << "           |     "<< (l) <<"        " << (m) <<"        |" << endl;
    os << "           |                       |" << endl;
    os << "  +--+--+--+--+--+--+     +--+--+--+--+--+" << endl;
    os << " "<< (h) <<"              |  "<< (i) <<"     |  "<< (j) <<"        "<< (k) <<" |" << endl;
    os << "                 |        |              |" << endl;
    os << "        +--+--+--+--+--+  |     +--+--+--+--+--+"<< endl;
    os << "        "<< (e) <<"              "<< (f) <<"  |     "<< (g) <<"              |" << endl;
    os << "                          |                    |" << endl;
    os << "              +--+--+--+--+--+  +--+--+--+--+--+--+" << endl;
    os << "              "<< (a) <<"              "<< (b) <<" "<< (c) <<"                 "<< (d) << endl;
    os << endl;

  }


  // Constructor for cloning s
  HangingWeights(bool share, HangingWeights& s) : Script(share,s) {
    x.update(*this, share, s.x);
    total.update(*this, share, s.total);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new HangingWeights(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("HangingWeights");

  opt.solutions(0);
  opt.icl(ICL_DOM);

  opt.parse(argc,argv);

  Script::run<HangingWeights,DFS,Options>(opt);
    
  return 0;
}


