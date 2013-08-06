/*

  Bus scheduling in Gecode.

  Problem from Taha "Introduction to Operations Research", page 58.
  This is a slightly more general model than Taha's.

  This model also experiments with combining optimal search (minimizing
  the value of num_buses) with a complete search. 
  See the comments for num_buses_arg below.
  

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/bus_scheduling.mzn
  * Comet   : http://www.hakank.org/comet/bus_scheduling.co

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class BusSchedule : public MinimizeScript {
protected:

  // number of time slots
  static const int time_slots = 6;
 
  // result: how many buses start the schedule at time slot t?
  IntVarArray x; 

  // the objective to minimize: the total number of buses
  IntVar num_buses;

  // max number of buses per time slot to consider
  static const int max_num = 100; 


  /**
   * Experimental:
   *
   * To be able to search for all solutions with the 
   * optiomal number of buses (26 in this example), we
   * use the size option (the number argument to the program).
   * If num_buses_arg is > 0 then num_buses is set to this value.
   * Don't forget to use the DFS search strategy -search dfs
   *
   * Example: The following command line
   *
   *   bus_schedule -search dfs 26
   *
   * shows all 145 optimal solutions.
   *
   */
  const int num_buses_arg;

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  }
;

  BusSchedule(const SizeOptions& opt) 
    : 
    x(*this, time_slots, 0, max_num),
    num_buses(*this, 0, max_num),
    num_buses_arg(opt.size())

  {

    // demand: minimum number of buses at time t
    int demands[] = {8, 10, 7, 12, 4, 4};

    // meet the demands for this and the next time slot
    for(int i = 0; i < time_slots - 1; i++)
      rel(*this, x[i]+x[i+1] >= demands[i], opt.icl());
    
    // demand "around the clock"
    rel(*this, x[time_slots-1] + x[0] == demands[time_slots-1], opt.icl());

    // calculate number of buses (to minimize)
    linear(*this, x, IRT_EQ, num_buses, opt.icl());

    // For exploring all solutions, use 
    //     -search dfs
    if (num_buses_arg) {
      rel(*this, num_buses == num_buses_arg, opt.icl());
    }

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "num_buses: " << num_buses << std::endl;
    os << "x: " << x << std::endl;
    os << std::endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return num_buses;
  }


  // Constructor for cloning s
  BusSchedule(bool share, BusSchedule& s) : MinimizeScript(share,s), 
                                            num_buses_arg(s.num_buses_arg) {
    x.update(*this, share, s.x);
    num_buses.update(*this, share, s.num_buses);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BusSchedule(share,*this);
  }

};


int
main(int argc, char* argv[]) {
  SizeOptions opt("BusSchedule");
  opt.solutions(0);
  opt.iterations(20000);

  opt.search(BusSchedule::SEARCH_BAB);
  opt.search(BusSchedule::SEARCH_DFS, "dfs");
  opt.search(BusSchedule::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (opt.search() == BusSchedule::SEARCH_DFS) {
    Script::run<BusSchedule,DFS,SizeOptions>(opt);
  } else {
    MinimizeScript::run<BusSchedule,BAB,SizeOptions>(opt);
  }


  return 0;
}


