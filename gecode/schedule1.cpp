/*

  Scheduling in Gecode.

  Example from SICStus Prolog:
  http://www.sics.se/sicstus/docs/latest/html/sicstus/Cumulative-Scheduling.html#Cumulative%20Scheduling

  """
  Cumulative Scheduling

  This example is a very small scheduling problem. We consider seven
  tasks where each task has a fixed duration and a fixed amount of used
  resource:

  Task Duration Resource
   t1    16       2
   t2     6       9
   t3    13       3
   t4     7       7
   t5     5      10
   t6    18       1
   t7     4      11

  The goal is to find a schedule that minimizes the completion time for
  the schedule while not exceeding the capacity 13 of the resource. The
  resource constraint is succinctly captured by a cumulative/4
  constraint. Branch-and-bound search is used to find the minimal
  completion time. 

  This example was adapted from [Beldiceanu & Contejean 94]. 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/schedule1.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/schedule1.ecl

  Note that Gecode also have a more complete version of scheduling
  constraint: cumulatives(). 
  See
  http://www.gecode.org/doc-latest/reference/group__TaskModelScheduling.html
  Also, I blogged about cumulatives() in
  "Scheduling with the cumulatives constraint in Gecode"
  http://www.hakank.org/constraint_programming_blog/2009/06/scheduling_with_the_cumulative.html

  This current model, however, use a simpler version: cumulative().


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>


using namespace Gecode;


class Scheduling : public MinimizeScript {
protected:

  static const int n = 7; // length of array

  IntVarArray start_times;
  IntVarArray end_times;
  IntVar last_end_time;  // the last task (to minimize)

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  };

  // Actual model
  Scheduling(const SizeOptions& opt) : 
    start_times(*this, n, 1, 1000),
    end_times(*this, n, 1, 1000),
    last_end_time(*this, 1, 1000)
  {

    int _Ds[] = {16, 6,13, 7, 5,18, 4};
    IntArgs Ds(n, _Ds);

    int _Rs[] = { 2, 9, 3, 7,10, 1,11};
    IntArgs Rs(n, _Rs);

    max(*this, end_times, last_end_time, opt.icl());

    // calculate the end times
    for(int i = 0; i < n; i++) {
      rel(*this, end_times[i] == start_times[i] + Ds[i], opt.icl());
    }

    int capacity = 13;
    cumulative(*this, capacity, start_times, Ds, Rs);

    // all optimal solutions if the program is started with
    //    -search dfs
    if (opt.search() == SEARCH_DFS) {
      rel(*this, last_end_time == 23, opt.icl());
    }

    branch(*this, start_times, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, end_times, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  Scheduling(bool share, Scheduling& s) : MinimizeScript(share,s) {
    start_times.update(*this, share, s.start_times);
    end_times.update(*this, share, s.end_times);
    last_end_time.update(*this, share, s.last_end_time);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Scheduling(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "start_times    : " << start_times << std::endl;
    os << "end_times      : " << end_times << std::endl;
    os << "last_end_time: " << last_end_time << std::endl;
    os << std::endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return last_end_time;
  }


};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("Scheduling");
  opt.solutions(0);

  opt.search(Scheduling::SEARCH_BAB);
  opt.search(Scheduling::SEARCH_DFS, "dfs");
  opt.search(Scheduling::SEARCH_BAB, "bab");


  if (!opt.size()) 
    opt.size(4);

  opt.parse(argc,argv);

  if (opt.search() == Scheduling::SEARCH_DFS) {
    Script::run<Scheduling, DFS, SizeOptions>(opt);
  } else {
    MinimizeScript::run<Scheduling, BAB, SizeOptions>(opt);
  }

  return 0;
}


