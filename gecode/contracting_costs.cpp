/*
  
  Contracting costs problem in Gecode.

  From 
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  Contracting Costs from "Mathematical Puzzles of Sam Loyd, Volume 2",
  number 20.

  A contractor planning the construction of a house found that he would
  have to pay:

    * $ 1,100 to the paper hanger and the painter,
    * $ 1,700 to the painter and plumber,
    * $ 1,100 to the plumber and electrician,
    * $ 3,300 to the electrician and carpenter,
    * $ 5,300 to the carpenter and mason,
    * $ 3,200 to the mason and painter. 

  What does each man charge for his services?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/contractor_costs.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/contracting_costs.pl
  * ECLiPSe: http://www.hakank.org/eclipse/contracting_costs.ecl

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


class ContractingCosts : public Script {
protected:

  const static int m = 5300;

  IntVar Ph;
  IntVar Pa;
  IntVar Pl;
  IntVar El;
  IntVar Ca;
  IntVar Ma;

public:

  ContractingCosts(const Options& opt) 
    : 
    Ph(*this, 0, m),
    Pa(*this, 0, m),
    Pl(*this, 0, m),
    El(*this, 0, m),
    Ca(*this, 0, m),
    Ma(*this, 0, m)
  {

    rel(*this, 
        1100 == Ph + Pa && 
        1700 == Pa + Pl && 
        1100 == Pl + El && 
        3300 == El + Ca && 
        5300 == Ca + Ma && 
        3200 == Ma + Pa
        );

    // branching
    branch(*this, Ph, INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Ph: " << Ph << endl;
    os << "Pa: " << Pa << endl;
    os << "Pl: " << Pl << endl;
    os << "El: " << El << endl;
    os << "Ca: " << Ca << endl;
    os << "Ma: " << Ma << endl;
    os << endl;

  }


  // Constructor for cloning s
  ContractingCosts(bool share, ContractingCosts& s) : Script(share,s) {
    Ph.update(*this, share, s.Ph);
    Pa.update(*this, share, s.Pa);
    Pl.update(*this, share, s.Pl);
    El.update(*this, share, s.El);
    Ca.update(*this, share, s.Ca);
    Ma.update(*this, share, s.Ma);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ContractingCosts(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("ContractingCosts");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<ContractingCosts,DFS,Options>(opt);
    
  return 0;
}


