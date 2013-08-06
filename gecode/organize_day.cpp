/*

  Simple Operations Research problem in Gecode.
  Organizing a day.
  
  Problem formulation from ECLiPSe
  Slides on (finite domain) Constraint Logic Programming, page 38f
  http://eclipse-clp.org/reports/eclipse.ppt


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/organize_day.mzn
  * Comet: http://www.hakank.org/comet/organize_day.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;



// 
// no_overlap: Requires that the two tasks 1 and 2 do not overlap.
//             starts: s1 and s2
//             durations: d1 and d2
//           
//
void no_overlap(Space& space, IntVar s1, int d1, IntVar s2, int d2, IntConLevel icl = ICL_BND) {
  rel(space, (s1 + d1 <= s2) || (s2 + d2 <= s1), icl);
}


class Organize : public Script {
protected:

  static const int n = 4;
  IntVarArray begins; // When the tasks start
  IntVarArray ends;   // When the tasks end

public:

  // The tasks
  enum {Work, 
        Mail, 
        Shop, 
        Bank};

  Organize(const Options& opt) 
    : 
    begins(*this, 4, 9, 17),
    ends(*this,   4, 9, 17)
  {

    // duration of each task
    int _durations[] = {4,1,2,1};
    IntArgs durations(n, _durations);

    std::cout << "durations: " << durations << std::endl;

    //
    // dependencies of the tasks:
    // task1 must start before task2
    //
    int num_dependencies = 2;
    int before_tasks[] = 
      {Bank, Shop,
       Mail, Work
      };

    //
    // handle dependencies of tasks
    //
    for(int i = 0; i < num_dependencies; i++) {
      rel(*this, ends[before_tasks[i*2]] <= begins[before_tasks[i*2+1]], opt.icl());
    }

    //
    // relation between start, duration and end of task
    //
    for(int i = 0; i < n; i++) {
      rel(*this, ends[i] == begins[i] + durations[i], opt.icl()); 
    }

    //
    // no overlapping of the taska
    //
    for(int i = 0; i < n; i++) {
      for(int j = i+1; j < n; j++) {
        no_overlap(*this, begins[i], durations[i], begins[j], durations[j]);
      }
    }

    rel(*this, begins[0], IRT_GQ, 11, opt.icl());

    branch(*this, begins, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, ends, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "begins: " << begins << std::endl;
    os << "ends  : " << ends << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Organize(bool share, Organize& s) : Script(share,s)
  {
    begins.update(*this, share, s.begins);
    ends.update(*this, share, s.ends);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Organize(share,*this);
  }
};


int
main(int argc, char* argv[]) {


  Options opt("Organize");
  opt.solutions(0);
  opt.parse(argc,argv);
  Script::run<Organize,DFS,Options>(opt);

  return 0;
}


