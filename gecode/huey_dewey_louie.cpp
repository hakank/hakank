/*

  Huey, Dewey and Louie problem in Gecode.
 
  From Marriott & Stucket, Programming with Constraints, page 42
  """
  Huey, Dewey and Louie are being questioned by their uncle. These are the 
  statements the make:
   Huey: Dewey and Louie has equal share in it; if one is quitly, 
         so is the other.
   Dewey: If Huey is guilty, then so am I.
   Louie: Dewey and I are not both quilty.
  
  Their uncle, knowing that they are cub scouts, realises that they 
  cannot tell a lie.
  Has he got sufficient information to decide who (if any) are quilty?
  """

  Here 1 means guilty, and 0 means not guilty.
  Solution:
     dewey = 0
     huey = 0
     louie = 0
  i.e. no one is guitly.

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/huey_dewey_louie.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/huey_dewey_louie.pl
  * ECLiPSe: http://www.hakank.org/eclipse/huey_dewey_louie.ecl

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


class Problem : public Script {

protected:

  BoolVar huey;
  BoolVar dewey;
  BoolVar louie;

  IntVarArray x;

public:

  Problem(const SizeOptions& opt) 
    : 
    huey(*this, 0,1),
    dewey(*this, 0,1),
    louie(*this, 0,1),
    x(*this, 3, 0, 1)
  {

    rel(*this, x[0] == huey);
    rel(*this, x[1] == dewey);
    rel(*this, x[2] == louie);
    
    //  Huey: Dewey and Louie has equal share in it; 
    //        if one is quitly, so is the other.
    rel(*this, (dewey == louie));
  
     // Dewey: If Huey is guilty, then so am I.
    rel(*this, (huey) >> (dewey));

    // Louie: Dewey and I are not both quilty.
    rel(*this, !(dewey && louie));

    // branching
    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());
 
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;

  }


  // Constructor for cloning s
  Problem(bool share, Problem& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Problem(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("Problem");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Problem,DFS,SizeOptions>(opt);    

  return 0;
}


