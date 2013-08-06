/*
  
  Monks and doors problem in Gecode.

 
  From http://user.it.uu.se/~rolandb/LP/gammal/960615_facit.ps
  """
  There is a room with four doors and eight monks. One or more of
  the doors may be exit. Each monk is either telling a lie or the truth.
 
  The monks make the following statements:
  Monk 1: Door A is the exit.
  Monk 2: At least one of the doors B and C is the exit.
  Monk 3: Monk 1 and Monk 2 are telling the truth.
  Monk 4: Doors A and B are both exits.
  Monk 5: Doors A and B are both exits.
  Monk 6: Either Monk 4 or Monk 5 is telling the truth.
  Monk 7: If Monk 3 is telling the truth, so is Monk 6.
  Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
 
  Which door is an exit no matter who is a liar and who is telling the
  truth.
  """
 
  Answer: Door A is an exit.
          And monks 1, 7, and 8 are telling the truth.

  Here is the output from this model:
    monks: {1, 0, 0, 0, 0, 0, 1, 1}
    doors: {1, 0, 0, 0}


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/monks_and_doors.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/monks_and_doors.pl
  * ECLiPSe: http://www.hakank.org/eclipse/monks_and_doors.ecl

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


class MonksAndDoors : public Script {
protected:

  const static int num_doors = 4;
  const static int num_monks = 8;

  BoolVarArray doors;
  BoolVarArray monks;

public:

  MonksAndDoors(const Options& opt) 
    : 
    doors(*this, num_doors, 0, 1),
    monks(*this, num_monks, 0, 1)
  {

    BoolVar 
      A(doors[0]), 
      B(doors[1]), 
      C(doors[2]),
      D(doors[3]);

    BoolVar
      M1(monks[0]),
      M2(monks[1]),
      M3(monks[2]),
      M4(monks[3]),
      M5(monks[4]),
      M6(monks[5]),
      M7(monks[6]),
      M8(monks[7]);


    // Monk 1: Door A is the exit.
    rel(*this,
        M1 == A
        );
    // Monk 2: At least one of the doors B and C is the exit.
    rel(*this,
        (M2 == (((B + C)) >= 1))
        );

    // Monk 3: Monk 1 and Monk 2 are telling the truth.
    rel(*this, M3 == (M1 && M2));

    // Monk 4: Doors A and B are both exits.
    rel(*this, M4 == (A && B));

      // Monk 5: Doors A and C are both exits.
    rel(*this, M5 == (A && C));

    // Monk 6: Either Monk 4 or Monk 5 is telling the truth.
    rel(*this, M6 == (M4 || M5));

    // Monk 7: If Monk 3 is telling the truth, so is Monk 6.
    rel(*this, M7 == (M3 >> M6));

    // Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
    rel(*this, (M8 == ((M7 && M8) >> M1)));
  
    // Exactly one door is an exit.
    rel(*this, (A + B + C + D) == 1);


    // branching
    branch(*this, monks, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, doors, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "monks: " << monks << endl;
    os << "doors: " << doors << endl;
  }


  // Constructor for cloning s
  MonksAndDoors(bool share, MonksAndDoors& s) : Script(share,s) {
    monks.update(*this, share, s.monks);
    doors.update(*this, share, s.doors);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new MonksAndDoors(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("MonksAndDoors");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<MonksAndDoors,DFS,Options>(opt);
    
  return 0;
}


