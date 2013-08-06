/*
  
  Five brigands problem in Gecode.

  From http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  The Five Brigands    from "Amusements in Mathematics, Dudeney",
  number 133.

  The five Spanish brigands, Alfonso, Benito, Carlos, Diego, and Esteban,
  were counting their spoils after a raid, when it was found that they
  had captured altogether exacly 200 doubloons. One of the band pointed
  out that if Alfonso had twelve times as much, Benito three times as
  much, Carlos the same amount, Diego half as much, and Esteban one-
  third as much, they would still have altogether just 200 doubloons.
  How many doubloons had each?

  There are a good many equally correct answers to this problem. The
  puzzle is to discover exactly how many different answers there are, it
  being understood that every man had something and there is to be no
  fractional money. 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/five_brigades.mzn
  * ECLiPSE: http://www.hakank.org/eclipse/five_brigands.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/five_brigands.pl

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


class FiveBrigands : public Script {
protected:

  IntVarArray LD;

public:

  FiveBrigands(const Options& opt) 
    : 
    LD(*this, 5, 8,160)
  {

    IntVar
      A(LD[0]),
      B(LD[1]),
      C(LD[2]),
      D2(LD[3]),
      E3(LD[4]);

    dom(*this, A, 8, 160);
    dom(*this, B, 8, 160);
    dom(*this, C, 8, 160);
    dom(*this, D2, 8, 100);
    dom(*this, E3, 8, 66);

    rel(*this, 
        A + B + C + 2*D2 + 3*E3  == 200 &&
        A * 12 + B * 3 + C + D2 + E3  == 200
        );

    // branching
    branch(*this, LD, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "LD: " << LD << endl;
  }


  // Constructor for cloning s
  FiveBrigands(bool share, FiveBrigands& s) : Script(share,s) {
    LD.update(*this, share, s.LD);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new FiveBrigands(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("FiveBrigands");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<FiveBrigands,DFS,Options>(opt);
    
  return 0;
}


