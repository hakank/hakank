/*
  
  Number of days problem (knapsack) in Gecode.

  From Nathan Brixius
  "Solving a Knapsack problem with Solver Foundation and LINQ"
  http://blogs.msdn.com/natbr/archive/2010/05/06/solving-a-knapsack-problem-with-solver-foundation-and-linq.aspx
 """
  Let's say I have this list of days and prices:

    List<ReservationPrice> prices = new List<ReservationPrice>(); 
    prices.Add(new ReservationPrice { NumberOfDays = 1, Price = 1000 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 2, Price = 1200 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 3, Price = 2500 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 4, Price = 3100 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 7, Price = 4000 }); 

  What I would like to able to do now is: give me the best price 
  from the list based on a number of days.

  So if ask for 3 days the best price from the list is from child one 
  (1000) and two (1200), but there are of course different combinations. 
  How would an algorithm that found the best price from this list 
  look like ?
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/number_of_days.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/number_of_days.ecl

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


class MusicMen : public MinimizeScript {
protected:

  const static int num_days = 5;

  IntVarArray x;
  IntVar days;
  IntVar total_cost;

public:

  MusicMen(const Options& opt) 
    : 
    x(*this, num_days, 0, 1),
    days(*this, 0, 1000),
    total_cost(*this, 0, 10000)
  {

    int _data_days[] = {1,2,3,4,7};
    int _data_costs[] = {1000,1200,2500,3100,4000};

    IntArgs data_days(num_days, _data_days);
    IntArgs data_costs(num_days, _data_costs);

    linear(*this, data_costs, x, IRT_EQ, total_cost);
    linear(*this, data_days, x, IRT_EQ, days);
   
    rel(*this, days == 13);


    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << endl;
    os << "days: " << days << endl;
    os << "total_cost: " << total_cost << endl;
    os << endl;
  }


  // Constructor for cloning s
  MusicMen(bool share, MusicMen& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    days.update(*this, share, s.days);
    total_cost.update(*this, share, s.total_cost);
  }

  virtual IntVar cost(void) const {
    return total_cost;
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new MusicMen(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("MusicMen");

  opt.solutions(0);

  opt.parse(argc,argv);

  MinimizeScript::run<MusicMen,BAB,Options>(opt);
    
  return 0;
}


