/*

  Twin letters problem in Gecode.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  Twin Letters    

  In the following puzzle, there are ten pairs of
  letters to be assigned to the same digit so that the multiplication
  (including intermediate results) is correct. Can you find out the
  pairs and their values?

          A B C
   *      D E F
   ____________
          G H I
        J K L
      M N O
   ____________
      P Q R S T
  """

  Compare with my other models:
  * ECLiPSe : http://www.hakank.org/eclipse/twin_letters.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/twin_letters.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class TwinLetters : public Script {
protected:

  static const int n = 20;

  IntVarArray LD;

public:

  TwinLetters(const Options& opt) 
  : 
    LD(*this, n, 0, 9)
  {

    IntVar 
      A(LD[0]),
      B(LD[1]),
      C(LD[2]),
      D(LD[3]),
      E(LD[4]),
      F(LD[5]),
      G(LD[6]),
      H(LD[7]),
      I(LD[8]),
      J(LD[9]),
      K(LD[10]),
      L(LD[11]),
      M(LD[12]),
      N(LD[13]),
      O(LD[14]),
      P(LD[15]),
      Q(LD[16]),
      R(LD[17]),
      S(LD[18]),
      T(LD[19]);

    IntVar C1(*this, 0,1);
    IntVar C2(*this, 0,2);
    IntVar C3(*this, 0,1);

    // exact 2 occurrences of each digit
    for(int i = 0; i <= 9; i++) {
      count(*this, LD, i, IRT_EQ, 2, opt.icl());
    }

    rel(*this,
                 100*G + 10*H + I +
        1000*J + 100*K + 10*L +
        10000*M + 1000*N + 100*O ==
        10000*P + 1000*Q + 100*R + 10*S + T      &&
        
        (100*D + 10*E + F)*C == 100*G + 10*H + I && 
        (100*D + 10*E + F)*B == 100*J + 10*K + L &&
        (100*D + 10*E + F)*A == 100*M + 10*N + O &&
        
        (100*A + 10*B + C) * (100*D + 10*E + F) ==
        10000*P + 1000*Q + 100*R + 10*S + T
        );

    // carry restrictions
    rel(*this, 
        T         == I              &&
        S + 10*C1 == H + L          &&
        R + 10*C2 == G + K + O + C1 &&
        Q + 10*C3 == J + N + C2     &&
        P         == M + C3
        );

    branch(*this, LD, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "LD    : " << LD << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  TwinLetters(bool share, TwinLetters& s) : Script(share,s) {
    LD.update(*this, share, s.LD);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new TwinLetters(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("TwinLetters");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<TwinLetters,DFS,Options>(opt);

  return 0;
}


