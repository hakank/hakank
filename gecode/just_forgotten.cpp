/*

  Just forgotten problem (Enigma 1517) in Gecode.
 
  From http://www.f1compiler.com/samples/Enigma%201517.f1.html
  """
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
  
  Joe was furious when he forgot one of his bank account numbers. 
  He remembered that it had all the digits 0 to 9 in some order, so he tried
  the following four sets without success:

      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7 
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5

  When Joe finally remembered his account number, he realised that in each set
  just four of the digits were in their correct position and that, if one knew
  that, it was possible to work out his account number.
  What was it?
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/just_forgotten.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/just_forgotten.pl
  * ECLiPSe: http://www.hakank.org/eclipse/just_forgotten.ecl

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


class JustForgotten : public Script {

protected:

  static const int rows = 4;
  static const int cols = 10;
  IntVarArray x;   // the grid

public:

  JustForgotten(const SizeOptions& opt) 
    : 
    x(*this, cols, 0, 9)
  {

    distinct(*this, x);

    int _a[] = {
      9,4,6,2,1,5,7,8,3,0,
      8,6,0,4,3,9,1,2,5,7,
      1,6,4,0,2,9,7,8,5,3,
      6,8,2,4,3,1,9,0,7,5
    };

    IntArgs a(rows*cols, _a);

    // Compared to the F1 model, this is slightly simpler
    for(int r = 0; r < rows; r++) {
      BoolVarArgs this_row;
      for(int c = 0; c < cols; c++) {
        IntVar v(*this, a[r*cols+c], a[r*cols+c]);
        this_row << expr(*this, x[c] == v);
      }
      rel(*this, sum(this_row) == 4);
    }
      
    // branching
    branch(*this, x, INT_VAR_DEGREE_MAX(), INT_VAL_MAX());
 
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;

  }


  // Constructor for cloning s
  JustForgotten(bool share, JustForgotten& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new JustForgotten(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("JustForgotten");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<JustForgotten,DFS,SizeOptions>(opt);    

  return 0;
}


