/*

  Torn number problem in Gecode.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/torn.html?19

  """
  The Torn Number from "Amusements in Mathematics, Dudeney", number 113

  I had the other day in my possession a label bearing the number 3025
  in large figures. This got accidentally torn in half, so that 30 was
  on one piece and 25 on the other. On looking at these pieces I began
  to make a calculation, scarcely concious of what I was doing, when I
  discovered this little peculiarity. If we add the 30 and the 25
  together and square the sum we get as the result the complete original
  number on the label! Now, the puzzle is to find another number,
  composed of four figures, all different, which may be divided in the
  middle and produce the same result. 
  """


  Compare with my other models:
  * MiniZinc: http://www.hakank.org/minizinc/torn_number.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/torn_numbers.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/torn_numbers.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class TornNumber : public Script {
protected:

  static const int n = 4;

  IntVarArray x;

public:

  TornNumber(const Options& opt) 
  : 
    x(*this, n, 0, 9)
  {

    IntVar
      D0(x[0]),
      D1(x[1]),
      D2(x[2]),
      D3(x[3]);

    IntVar Sum(*this, 0, 1000);

    distinct(*this, x, opt.icl());

    rel(*this, D3 != 0);
    rel(*this, Sum == D3 * 10 + D2 + D1 * 10 + D0, opt.icl());
    rel(*this, Sum * Sum == D3 * 1000 + D2 * 100 + D1 * 10 + D0, opt.icl());
    // none of the bits are 30
    rel(*this, !(30 == D3*10 + D2  || 30 == D1*10 + D0), opt.icl());
    // none of the bits are 25
    rel(*this, !(25 == D3*10 + D2  || 25 == D1*10 + D0), opt.icl());
 
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  TornNumber(bool share, TornNumber& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new TornNumber(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("TornNumber");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<TornNumber,DFS,Options>(opt);

  return 0;
}


