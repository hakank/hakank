/*

  Moving furnitures (cumulative) in Gecode.
  
  Problem from Marriott & Stuckey: 'Programming with constraints', page  112f
  The problem is to move furnitures from an apartment.
 

  One solution of the minimization problem:
  NumPersons: 3
  piano: 30 
  chair: 15
  bed: 0
  table: 15
  

  See my other models:
  MiniZinc: http://www.hakank.org/minizinc/furnitute_moving.mzn
  Comet: http://www.hakank.org/comet/furniture_moving.co
         http://www.hakank.org/comet/furniture_moving_cumulative.co
  JaCoP: http://www.hakank.org/JaCoP/FurnitureMoving.java
  Choco: http://www.hakank.org/choco/FurnitureMoving.java

  Note: The above models use the "old" cumulative constraint.
        Since Gecode have the more general version cumulatives, this model
        is somewhat different.


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

/*

  One result:

  z: 45
  machines   : {0,   0,  0,  0,     0,  0, 0}
  start_times: {30,  0,  0, 15,     0,  0, 0}
  durations  : {30, 10, 15, 15,    10, 60, 60}
  end_times  : {60, 10, 15, 30,    10, 60, 60}

  Note: durations of persons - the three last elements - is variables, 
        so we can see that one person only need to work 10 minutes, 
        the other two must work a full hour.

 */

#include <gecode/driver.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class Furniture : public MinimizeScript {
protected:

  static const int num_tasks = 4;
  static const int num_persons = 3;
  static const int n = num_tasks + num_persons; // number of tasks (4) + persons (3)

  IntVarArray durations;    // durations, 
                            //      check if we have to use all person's time
  IntVarArray start_times;  // start times
  IntVarArray end_times;    // end times

  IntVarArray machines;     // the machine to be assigned

  IntVar z;                 // number of persons needed, to minimize

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB
  }
;

  Furniture(const Options& opt) 
    :
    durations(*this, n, 0, 60),
    start_times(*this, n, 0, 60),
    end_times(*this, n, 0, 60),
    machines(*this, n, 0, 0), // only one machine
    z(*this, 0, n*60)
  {


    // The furniture moving as a task covering model:
    // This is modeled as model G from 
    // "A new multi-resource cumulatives constraint with negative heights"
    // The tasks are "consumers" (negative height) and the persons 
    // are "producers" (positive height)
    //
    //                     Tasks                     Persons
    // note: we leave duration for persons free since we want to see 
    //       what we really need for duration of persons
    int _durations[] = {   30,    10,  15,   15                };
    int _height[] =    {   -2,    -1,  -2,   -2,      1,  1,  1};
    int _limit[] =     {0}; // just one machine

    // tasks

    // duration for the tasks
    for(int i = 0; i < num_tasks; i++) {
      rel(*this, durations[i], IRT_EQ, _durations[i], opt.icl());
    }
    
    // duration for persons, handle symmetry of persons
    // person 1 must work less or equal amount of 
    // work as person 2 etc.
    for(int i = num_tasks+1; i < n; i++) {
      rel(*this, durations[i-1], IRT_LQ, durations[i], opt.icl());
    }


    IntArgs height(n, _height);

    // machines, just one machine
    IntArgs limit(1, _limit);

    // we use the at least 0 variant here
    bool atmost = false;
    cumulatives(*this, machines, start_times, durations, end_times, height,
                limit, atmost, opt.icl());

    // calculate z (to minimize)
    // linear(*this, machines, IRT_EQ, z, opt.icl());
    // linear(*this, end_times, IRT_EQ, z, opt.icl());
    // linear(*this, durations, IRT_EQ, z, opt.icl());
    linear(*this, end_times, IRT_EQ, z, opt.icl());

    if (opt.search() == Furniture::SEARCH_DFS) {
      rel(*this, z <= 245, opt.icl());
    }

    branch(*this, machines, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, end_times, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, start_times, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, durations, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "z          : " << z << std::endl;
    os << "machines   : " << machines << std::endl;
    os << "start_times: " << start_times << std::endl;
    os << "durations  : " << durations << std::endl;
    os << "end_times  : " << end_times << std::endl;

  }

  // Return cost
  virtual IntVar cost(void) const {
    return z;
  }


  // Constructor for cloning s
  Furniture(bool share, Furniture& s) : MinimizeScript(share,s) {
    start_times.update(*this, share, s.start_times);
    end_times.update(*this, share, s.end_times);
    durations.update(*this, share, s.durations);
    machines.update(*this, share, s.machines);
    z.update(*this, share, s.z);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Furniture(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Furniture");
  opt.solutions(0);
  opt.iterations(200);
  opt.search(Furniture::SEARCH_BAB);
  opt.search(Furniture::SEARCH_DFS, "dfs");
  opt.search(Furniture::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (opt.search() == Furniture::SEARCH_DFS) {
    Script::run<Furniture, DFS, Options>(opt);
  } else {
    MinimizeScript::run<Furniture, BAB, Options>(opt);
  }

  return 0;
}


