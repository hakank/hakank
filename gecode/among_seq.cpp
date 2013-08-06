/*

  Global constraint among_seq in Gecode.

  From Global constraint catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong_seq.html
  """
  Constraint

    among_seq(LOW,UP,SEQ,VARIABLES,VALUES)

  Purpose  
  Constrains all sequences of SEQ consecutive variables of the collection 
  VARIABLES to take at least LOW values in VALUES and at most UP values 
  in VALUES.

  Example
    (
    1,2,4,‹9,2,4,5,5,7,2>
    <0,2,4,6,8>
    )

  The among_seq constraint holds since the different sequences of 4 
  consecutive variables contains respectively 2, 2, 1 and 1 even numbers.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/among_seq.mzn
  * Comet   : http://www.hakank.org/comet/among_seq.co
  * ECLiPSe: http://www.hakank.org/eclipse/among_seq.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/among_seq.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

void among_seq(Space& space, IntVar low, IntVar up, int seq, IntVarArray x, IntVarArray y) {
  int n = x.size();
  int m = y.size();

  for(int i = 0; i < n-seq+1; i++) {
    BoolVarArgs s;
    for(int j = i; j < i+seq; j++) {
      for(int k = 0; k < m; k++) {
        s << expr(space, x[j] == y[k]);
      }
    }
    rel(space, sum(s) >= low);
    rel(space, sum(s) <= up);
  }

}

class AmongSeq : public Script {
protected:

  static const int n = 7;
  static const int m = 5;
  static const int seq = 4;

  IntVarArray x;
  IntVarArray y;
  IntVar low;
  IntVar up;

public:

  AmongSeq(const Options& opt) 
  : 
    x(*this, n, 1, 9),
    y(*this, m, 0, 8),
    low(*this, 0,9),
    up(*this, 0,9)
  {
    int _x[] = {9, 2, 4, 5, 5, 7, 2};
    for(int i = 0; i < n; i++) {
      rel(*this, x[i] == _x[i]);
    }

    int _y[] = {0, 2, 4, 6, 8};
    /*
    for(int j = 0; j < m; j++) {
      rel(*this, y[j] == _y[j]);
    }
    */

    rel(*this, low == 1);
    rel(*this, up == 2);

    among_seq(*this, low, up, seq, x, y);

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, y, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "y: " << y << std::endl;
    os << "low: " << low << std::endl;
    os << "up: " << up << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  AmongSeq(bool share, AmongSeq& s) : Script(share,s) {
    x.update(*this, share, s.x);
    y.update(*this, share, s.y);
    low.update(*this, share, s.low);
    up.update(*this, share, s.up);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AmongSeq(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("AmongSeq");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<AmongSeq,DFS,Options>(opt);

  return 0;
}


