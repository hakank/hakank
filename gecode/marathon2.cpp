/*
  
  Marathon puzzle in Gecode.

  From Xpress example
  http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
  """
  Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
  have arrived as the first six at the Paris marathon.
  Reconstruct their arrival order from the following
  information:
  a) Olivier has not arrived last
  b) Dominique, Pascal and Ignace have arrived before Naren
     and Olivier
  c) Dominique who was third last year has improved this year.
  d) Philippe is among the first four.
  e) Ignace has arrived neither in second nor third position.
  f) Pascal has beaten Naren by three positions.
  g) Neither Ignace nor Dominique are on the fourth position.
  
     (c) 2002 Dash Associates
    author: S. Heipcke, Mar. 2002
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/marathon2.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/marathon2.ecl
  * ECLiPSe: http://www.hakank.org/eclipse/marathon2.ecl

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


class Marathon2 : public Script {
protected:

  static const int n = 6;

  IntVarArray runners;
  IntVarArray positions;

public:

  Marathon2(const Options& opt) 
    : 
    runners(*this, n, 0, n-1),
    positions(*this, n, 0, n-1)
  {

    IntVar
      Dominique(runners[0]),
      Ignace(runners[1]),
      Naren(runners[2]),
      Olivier(runners[3]),
      Philippe(runners[4]),
      Pascal(runners[5]);
    
    distinct(*this, runners, opt.icl());

    // Note: Since we use channel the positions are 
    //       0..n-1.

    // a: Olivier not last
    rel(*this, Olivier    != n-1);

    // b: Dominique, Pascal and Ignace before Naren and Olivier
    rel(*this, Dominique  < Naren );
    rel(*this, Dominique  < Olivier);
    rel(*this, Pascal     < Naren);
    rel(*this, Pascal     < Olivier);
    rel(*this, Ignace     < Naren);
    rel(*this, Ignace     < Olivier); 

    // c: Dominique better than third
    rel(*this, Dominique  < 2);

    // d: Philippe is among the first four
    rel(*this, Philippe   <= 3); 

    // e: Ignace neither second nor third
    rel(*this, Ignace     != 1);
    rel(*this, Ignace     != 2);

    // f: Pascal three places earlier than Naren
    rel(*this, Pascal + 3 == Naren);  

    // g: Neither Ignace nor Dominique on fourth position
    rel(*this, Ignace     != 3);
    rel(*this, Dominique  != 3);

    // channeling runners <-> positions
    channel(*this, runners, positions);

    // branching
    branch(*this, runners, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "runners: " << runners << endl;
    os << "positions: " << positions << endl;
    os << endl;

  }


  // Constructor for cloning s
  Marathon2(bool share, Marathon2& s) : Script(share,s) {
    runners.update(*this, share, s.runners);
    positions.update(*this, share, s.positions);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Marathon2(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Marathon2");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Marathon2,DFS,Options>(opt);
    
  return 0;
}


