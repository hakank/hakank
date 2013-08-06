/*
  
  Covering problem in Gecode.

  This example is from the OPL example covering.mod
  """
  Consider selecting workers to build a house. The construction of a 
  house can be divided into a number of tasks, each requiring a number of 
  skills (e.g., plumbing or masonry). A worker may or may not perform a 
  task, depending on skills. In addition, each worker can be hired for a 
  cost that also depends on his qualifications. The problem consists of 
  selecting a set of workers to perform all the tasks, while minimizing the 
  cost. This is known as a set-covering problem. The key idea in modeling 
  a set-covering problem as an integer program is to associate a 0/1 
  variable with each worker to represent whether the worker is hired. 
  To make sure that all the tasks are performed, it is sufficient to 
  choose at least one worker by task. This constraint can be expressed by a 
  simple linear inequality.
  """

  Solution from the OPL model:
  """
  Optimal solution found with objective: 14
  crew= {23 25 26}
  """


  Compare with the following models:
  * Comet: http://www.hakank.org/comet/covering_opl.co
  * SICStus Prolog: http://www.hakank.org/sicstus/covering_opl.pl
  * ECLiPSe: http://www.hakank.org/eclipse/covering_opl.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/set.hh>
#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class CoveringOPL : public MinimizeScript {
protected:

  const static int nbWorkers = 32;

  IntVarArray Hire;
  // cost: to minimize
  IntVar Cost;

public:

  CoveringOPL(const Options& opt) 
    : 
    Hire(*this, nbWorkers, 0, 1),
    Cost(*this, 0, 99)
  {

    int _Costs[] = {1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 
                    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9 };
    IntArgs Costs(nbWorkers, _Costs);

    // Who is qualified for a task?
    int num_tasks = 15;
    int _Qualified[] = 
      {
         7,    1,  9, 19, 22, 25, 28, 31,
        11,    2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32,
         7,    3, 10, 19, 24, 26, 30, 32,
         5,    4, 21, 25, 28, 32,
         7,    5, 11, 16, 22, 23, 27, 31,
         6,    6, 20, 24, 26, 30, 32,
         6,    7, 12, 17, 25, 30, 31,
         5,    8, 17, 20, 22, 23,
         7,    9, 13, 14, 26, 29, 30, 31,
         5,   10, 21, 25, 31, 32,
         8,   14, 15, 18, 23, 24, 27, 30, 32,
         7,   18, 19, 22, 24, 26, 29, 31,
         6,   11, 20, 25, 28, 30, 32,
         4,   16, 19, 23, 31,
         6,    9, 18, 26, 28, 31, 32
      };
    IntSet _QualifiedSet[num_tasks];
    int s = 0;
    for(int t = 0; t < num_tasks; t++) {
      int num = _Qualified[s++];
      IntArgs tmp;
      for(int i = 0; i < num; i++) {
        tmp << _Qualified[s++]-1; // to 0-based
      }
      _QualifiedSet[t] = IntSet(tmp);
    }
    
    /*
      Comet code: 
        forall(j in Tasks)
          m.post(sum( c in Qualified[j] ) 
              Hire[c] >= 1);
    */
    // we must hire at least one worker
    // qualified for each task
    for(int t = 0; t < num_tasks; t++) {
      IntSet tt(_QualifiedSet[t]);
      BoolVarArgs bb;
      for(int w = 0; w < nbWorkers; w++) {
        // is this worker qualified?
        if (tt.in(w)) {
          // then, is he/she hired?
          bb << expr(*this, Hire[w] == 1);
        }
      }
      // at least one worker must hired for this task
      rel(*this, sum(bb) >= 1);
    }

    // The total cost for the hired workers
    linear(*this, Costs, Hire, IRT_EQ, Cost);

    // branching
    branch(*this, Hire, INT_VAR_AFC_SIZE_MIN(), INT_VAL_SPLIT_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Cost: " << Cost << endl;
    os << "Hire these workers: " << endl;
    for(int w = 0; w < nbWorkers; w++) {
      if (Hire[w].val() == 1) {
        os << w << " ";
      }
    }
    os << endl;
    os << endl;

  }


  // Constructor for cloning s
  CoveringOPL(bool share, CoveringOPL& s) : MinimizeScript(share,s) {
    Cost.update(*this, share, s.Cost);
    Hire.update(*this, share, s.Hire);
  }

  virtual IntVar cost(void) const {
    return Cost;
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CoveringOPL(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("CoveringOPL");

  opt.solutions(0);

  opt.parse(argc,argv);

  MinimizeScript::run<CoveringOPL,BAB,Options>(opt);
    
  return 0;
}


