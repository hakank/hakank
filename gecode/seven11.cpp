/*
  
  7-11 puzzle in Gecode.

  From
  http://www-lp.doc.ic.ac.uk/UserPages/staff/ft/alp/humour/num/seven-1.html

  """
  Appeared in Volume 9/1, February 1996
  A man goes into a store and selects four items to purchase. He walks up to the
  counter to pay and the clerk says 
  "Hold on, my cash register is broken, so I have to use a calculator to get 
  your total... okay, that'll be $7.11" 
  The man pays, and as he is walking out, the clerk yells "Wait a second! I
  multiplied the prices together instead of adding them. Let me get
  the total again... hey, what do you know! It comes out the same!"

  What were the prices of the four items? All of them are of the form a.bc, that 
  is, we're dealing with standard U.S. money, no fractions of a cent. Ignore
  sales taxes too.

  Some discussion of this appeared in the sci.math newsgroup under the topic
  "8th grade math puzzler", since the puzzle was originally given to a class of
  8th Graders.

  John Shonder
  padrote@delphi.com
  """
  
  See also Martin Chlond's Integer Programming Puzzles:
  http://www.chlond.demon.co.uk/puzzles/puzzles4.html (puzzle nr. 12)

  
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/seven11.mzn
  * ECLiPSe   : http://www.hakank.org/eclipse/seven1.ecl

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


class Seven11 : public Script {
protected:

  static const int n = 4;

  IntVarArray L;

public:

  Seven11(const Options& opt) 
    : 
    L(*this, n, 0, 711)
  {

    IntVar
      A(L[0]), 
      B(L[1]), 
      C(L[2]),
      D(L[3]);

    IntVar S(*this, 711, 711);

    rel(*this,
        A * B * C * D == S*100*100*100 &&
        A + B + C + D == S
        );
     
    // symmetry breaking
    // rel(*this, L, IRT_LQ, ICL_DOM);
    rel(*this, 
        A <= B &&
        B <= C &&
        C <= D
        );


    // branching
    branch(*this, L, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "L: " << L << endl;
    os << endl;

  }


  // Constructor for cloning s
  Seven11(bool share, Seven11& s) : Script(share,s) {
    L.update(*this, share, s.L);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Seven11(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Seven11");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Seven11,DFS,Options>(opt);
    
  return 0;
}


