/*

  Some different models of cumulatives constraint in Gecode.

  This program implements the 8 different cumulatives models (A..H) from
  the paper by Nicolas Beldiceanu and Mats Carlsson: 
  "A new multi-resource cumulatives constraint with negative heights", 
  Principles and Practice of Constraint Programming 2002.
  ftp://ftp.sics.se/pub/SICS-reports/Reports/SICS-T--2001-11--SE.ps.Z

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


namespace {

  // List of problems
  extern const int* problems[];

  // Number of specifications
  extern const unsigned int n_examples;

}

class CumulativesExamples : public MinimizeScript {
protected:

  const int* prob;          // problem instance

  IntVarArray start_times;  // start times for tasks
  IntVarArray end_times;    // end times for tasks
  IntVarArray machines;     // the machine(s) to be assigned

  IntVar z;                 // sum of end times, to minimize

  // number of tasks from the problem instance
  int num_tasks(void) const {
    return prob[0];
  }

  // number of machines from the problem instance
  int num_machines(void) const {
    return prob[1];
  }


public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB
  };

  enum {A,B,C,D,E,F,G,H};

  CumulativesExamples(const SizeOptions& opt) 
    :
    prob(problems[opt.size()]),
    start_times(*this, num_tasks(), 0, 10),
    end_times(*this, num_tasks(), 0, 10),
    machines(*this, num_tasks(), 0, num_machines()-1), 
    z(*this, 0, 1000)
  {

    std::cout << "num_tasks   : " << num_tasks() << std::endl;
    std::cout << "num_machines: " << num_machines() << std::endl;

    bool atmost = true; // may be changed for the problem instances

    int _durations[num_tasks()];
    int _height[num_tasks()];
    int _limit[num_machines()];

    // durations
    int c = 2;
    for(int i = 0; i < num_tasks(); i++) {
      _durations[i] = prob[c++];

    }

    // height
    for(int i = 0; i < num_tasks(); i++) {
      _height[i] = prob[c++];
    }

    // limit
    for(int i = 0; i < num_machines();  i++) {
      _limit[i] = prob[c++];
    }

    // tasks
    IntArgs durations(num_tasks(), _durations);
    IntArgs height(num_tasks(), _height);

    // machines
    IntArgs limit(num_machines(), _limit);

    std::cout << "durations: " << durations << std::endl;
    std::cout << "height   : " << height << std::endl;
    std::cout << "limit    : " << limit << std::endl;
    std::cout << std::endl;

    /**
     * Example A (0) 
     * from "A new multi-resource cumulatives constraint with negative heights"
     * """
     * Part (A) is the classical original cumulative constraint described in [1]. In the
     * Resources parameter we have introduced one single resource, which has value 1 as
     * identifier and 4 as its maximum capacity.
     * """
     * Standard cumulative, one machine.
     * num_tasks = 4
     * num_machines = 1
     * 
     * int _durations[] = {2,2,1,1}; // tasks
     * int _height[]    = {2,1,1,3}; // tasks
     * int _limit[]     = {4};       // single machine
     * atmost = true;
     *
     */

    if (opt.size() == A) {
      std::cout << "Problem A" << std::endl;
      atmost = true;
    }
            

    /** Example B (1)
     * """  
     * Part (B) is an extension of the original cumulative constraint where 
     * we have more than one single resource.
     * """
     * Two machines.
     * num_tasks = 4
     * num_machines = 2
     * 
     * int _durations[] = {3,2,4,1}; // tasks
     * int _height[]    = {1,2,1,2}; // tasks
     * int _limit[]     = {3,1};     // 2 machines with different limits
     * atmost = true;
     *
     */
    if (opt.size() == B) {
      std::cout << "Problem B" << std::endl;
      atmost = true;
    }
    

    /** Example C (2)
     * """
     * Part (C) is the at least variant of the cumulative constraint available in 
     * CHIP. This variant enforces to reach a minimum level between the first and the 
     * last utilization of the resource. In order to express the previous condition 
     * we create a dummy task of height 0 (represented by the thick line between instants 
     * 2 and 5) for which the start and the end respectively correspond to the 
     * earliest start and to the latest end of the different tasks to schedule. 
     * For this purpose we respectively use a minimum and maximum6 constraints
     * """
     * This is an at least variant where there must be "covered" _at least_ 2 (if any)
     * num_tasks = 5
     * num_machines = 1
     *
     * int _durations[] = {4,4,2,2,1}; // tasks
     * int _height[]    = {0,1,1,2,1}; // tasks
     * int _limit[]     = {2};         // one machine
     * atmost = false;
     *
     */
    if (opt.size() == C) {
      std::cout << "Problem C" << std::endl;
      atmost = false;
    }


    /** Example D (3)
     * """
     * Part (D) is a new variant of the previous case where the "at least" 
     * constraint applies only for the instants that are overlapped by at 
     * least one task.
     * """
     * at least variant where there must be "covered" _at least_ 2 (if any)
     * 
     * Note: The solution places task 3 on top of task 1 and 2 which means
     *       that the full height is 2 + 1 + 4 = 7. The paper places tasks 3 after
     *       task 1 and 2 are done.
     *       How does one minimize this resulting height via cumulatives?
     *       Answer: By adding an atmost=true cumulatives with the limit of - say - 4.
     *
     * num_tasks = 3
     * num_machines = 1
     *
     * int _durations[] = {2,1,1}; // tasks
     * int _height[]    = {2,1,4}; // tasks
     * int _limit[]     = {2};     // machine
     * atmost = false;
     *
     */
    if (opt.size() == D) {
      std::cout << "Problem D" << std::endl;
      atmost = false;

      IntArgs limitD(1, 4);
      cumulatives(*this, machines, start_times, durations, end_times, height,
                  limitD, true, opt.icl());
    }


    /* Example E (4)
     * """
     * Part (E) is a producer-consumer problem where tasks 1,2 represent producers,
     * while tasks 3,4 are consumers. On one side, a producer task starts at the 
     * earliest start and produces a quantity equal to its height at a date 
     * that corresponds to its end. On the other side, a consumer task ends at the 
     * latest end and consumes a quantity equal to its height at a date that 
     * matches its start. The resource can be interpreted as a tank in which one 
     * adds or removes at specific points in time various quantities. The 
     * cumulatives constraint enforces that, at each instant, one does not 
     * consume more than what is currently available in the tank.
     * """
     * task 0 and 1 are producers and task 2 and 3 are consumers.
     * num_tasks = 4
     * num_machines = 1
     * 
     * int _durations[] = {3,4,6,2}; // tasks
     * int _height[]    = {2,3,1,4}; // tasks
     * int _limit[]     = {7};       // one machine
     * atmost = true;
     *
     */
    if (opt.size() == E) {
      std::cout << "Problem E" << std::endl;
      atmost = true;
      
      //
      // The trick is the following two requirements:
      //
      // "a producer task starts at the earliest start and produces..."
      // i.e. they _starts_ at 0 
      for(int i = 0; i <= 1; i++) {
        rel(*this, start_times[i], IRT_EQ, 0, opt.icl());
      }
      
      // "a consumer task ends at the latest end..."
      // i.e. they _ends_ at 6 (or some arbitrary end > 5)
      // for(int i = 2; i < num_tasks(); i++) {
      //  rel(*this, end_times[i], IRT_EQ, 6, opt.icl());
      // }
      // experimental: let the model find the latest end
      max(*this, end_times, z, opt.icl());

    }


    /** Example F (5)
     * """
     * Part (F) is a generalization of the previous producer-consumer problem where we
     * have two tanks. As for the previous example the cumulatives constraint enforces no
     * negative stocks on both tanks.
     * """
     * task 0, 1, and 3 are producers and task 2 and 4 are consumers.
     * num_tasks = 5
     * num_machines = 2
     *
     * int _durations[] = {1,3,4,3,4}; // tasks
     * int _height[]    = {1,2,1,1,2}; // tasks
     * int _limit[]     = {3,4}; // machines
     * atmost = true;
     *
     */
    if (opt.size() == F) {
      std::cout << "Problem F" << std::endl;
      atmost = true;

      // same principle as for example E:
      // producers starts at 0
      rel(*this, start_times[0], IRT_EQ, 0, opt.icl());
      rel(*this, start_times[1], IRT_EQ, 0, opt.icl());
      rel(*this, start_times[3], IRT_EQ, 0, opt.icl());
      
      // consumers ends at 6
      // rel(*this, end_times[2], IRT_EQ, 6, opt.icl());
      // rel(*this, end_times[4], IRT_EQ, 6, opt.icl());

      // experimental: we calculate the latest time
      max(*this, end_times, z, opt.icl());
      rel(*this, end_times[2], IRT_EQ, z, opt.icl());
      rel(*this, end_times[4], IRT_EQ, z, opt.icl());

    }


    /** Example G (6)
     * """
     * Part (G) describes a covering problem where one has to cover a 
     * given workload by a set of tasks. The workload can be interpreted 
     * as the number of persons required during specific time intervals, 
     * while a task can be interpreted as the work performed by a group 
     * of persons. The height of the initially fixed tasks (i.e. tasks 1
     * and 2) that represent the workload is modelled with negative numbers, 
     * while the height of the tasks related to the persons (i.e. tasks 
     * 3,4,5,6) is positive. The covering constraint is imposed by the fact 
     * that, at each point in time, the cumulatives constraint enforces the 
     * cumulated height, of the tasks that overlap this point, to be greater 
     * than or equal to 0: at each point in time the number of available 
     * persons should be greater than or equal to the required demand 
     * expressed by the work-load to cover. A thick line indicates the 
     * cumulated profile resulting from the negative and positive heights.
     * """
     * num_tasks = 6
     * num_machines = 1
     *
     * 0,1 are tasks (consumers with negative heights),   2,3,4 are persons (producers)
     * And the interesting part is how the two tasks should be arranged.
     * The persons are just put on top of each other, 
     * and there are no limit how they are "stacked", they just work (produce).
     * 
     * Also, see http://www.hakank.org/gecode/furniture_moving.cpp for a similiar
     * problem, the furniture moving problem from 
     * Marriott & Stuckey: "Programming with constraints"
     * 
     * int _durations[] = { 2, 2,  4,2,4,3}; // 2 tasks and 4 persons
     * int _height[]    = {-2,-3,  1,1,1,1}; // 2 tasks and 4 persons
     * int _limit[]     = {0}; // 1 machine
     * atmost = false;
     *
     */
    if (opt.size() == G) {
      std::cout << "Problem G" << std::endl;
      atmost = false;
    }
    

    /** Example H (7)
     * """
     * Finally, part (H) generalizes (G) by introducing 2 distinct workloads to cover.
     * """
     * Here 0,2,and 5 are tasks and the rest (1,3,4,6) are persons
     *
     * num_tasks = 7
     * num_machines = 2
     *                                0  1   2  3  4   5  6
     * int _durations[num_tasks] = {  2, 4,  2, 3, 2,  2, 4 }; // 3 tasks and 4 persons
     * int _height[num_tasks]    = { -2, 1, -1, 2, 3, -1, 1};  // 3 tasks and 4 persons
     * int _limit[num_machines]  = {0, 0}; // 2 machines
     * atmost = false
     *
     */
    if (opt.size() == H) {
      std::cout << "Problem H" << std::endl;
      atmost = false;

      // we don't accept end_times > 6
      rel(*this, end_times, IRT_LQ, 6, opt.icl());

      //
      // set the machines
      //
      // first machine
      for(int i = 0; i <= 4; i++) {
        rel(*this, machines[i], IRT_EQ, 0, opt.icl());
      }
      
      // second machine
      for(int i = 5; i <= 6; i++) {
        rel(*this, machines[i], IRT_EQ, 1, opt.icl());
      }

      // add atmost limit per machine
      IntArgs limitH(num_machines(), 2,1);
      cumulatives(*this, machines, start_times, durations, end_times, height,
                  limitH, true, opt.icl());

    }


    //
    // General constraints
    //
    cumulatives(*this, machines, start_times, durations, end_times, height,
                limit, atmost, opt.icl());


    // calculate z (to minimize)
    // linear(*this, machines, IRT_EQ, z, opt.icl());
    // linear(*this, start_times, IRT_EQ, z, opt.icl());
    if (opt.size() != E  && opt.size() != F) {
      linear(*this, end_times, IRT_EQ, z, opt.icl());
    }


    branch(*this, machines, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, end_times, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, start_times, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "z: " << z << std::endl;
    os << "machines   : " << machines << std::endl;
    os << "start_times: " << start_times << std::endl;
    os << "end_times  : " << end_times << std::endl;

  }

  // Return cost
  virtual IntVar cost(void) const {
    return z;
  }


  // Constructor for cloning s
  CumulativesExamples(bool share, CumulativesExamples& s) : MinimizeScript(share,s) {
    start_times.update(*this, share, s.start_times);
    end_times.update(*this, share, s.end_times);
    machines.update(*this, share, s.machines);
    z.update(*this, share, s.z);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CumulativesExamples(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("CumulativesExamples");
  opt.solutions(0);
  opt.iterations(200);
  opt.search(CumulativesExamples::SEARCH_BAB);
  opt.search(CumulativesExamples::SEARCH_DFS, "dfs");
  opt.search(CumulativesExamples::SEARCH_BAB, "bab");

  opt.size(0);

  opt.parse(argc,argv);

  if (opt.size() >= n_examples) {
    std::cout << "Problems are between 0..7 which corresponds to problem A..H in the paper." << std::endl;
    return -1;
  }

  if (opt.search() == CumulativesExamples::SEARCH_DFS) {
    Script::run<CumulativesExamples, DFS, SizeOptions>(opt);
  } else {
    MinimizeScript::run<CumulativesExamples, BAB, SizeOptions>(opt);
  }


  return 0;
}


namespace {

  /** Problem specifications
   *
   *  See above for details.
   *
   *  We code the problem instances with the following:
   *
   *   num_tasks, num_machines,
   *   <duration per task>
   *   <height per task>
   *   <limit per machine>
   * 
   */

  // Example A
  const int pA[] = 
    {
      4,1,
      2,2,1,1,
      2,1,1,3,
      4      
    };

  // Example B
  const int pB[] = 
    {
      4,2,
      3,2,4,1,
      1,2,1,2,
      3,1
     
    };

  // Example C
  const int pC[] = 
    {
      5,1,
      4,4,2,2,1,
      0,1,1,2,1,
      2
    };

  // Example D
  const int pD[] = 
    {
      3,1,
      2,1,1,
      2,1,4,
      2
    };

  // Example E
  const int pE[] = 
    {
      4,1,
      3,4,6,2,
      2,3,1,4,
      7
    };

  // Example F
  const int pF[] = 
    {
      5,2,
      1,3,4,3,4,
      1,2,1,1,2,
      3,4
    };

  // Example G
  const int pG[] = 
    {
      6,1,
       2, 2, 4,2,4,3,
      -2,-3, 1,1,1,1,
      0
    };

  // Example H
  const int pH[] = 
    {
      7,2,
      2,  4,  2, 3, 2,  2, 4,
      -2, 1, -1, 2, 3, -1, 1,
      0, 0
    };

  const int *problems[] = {pA, pB, pC, pD, pE, pF, pG, pH};
  const unsigned n_examples = sizeof(problems)/sizeof(int*);

}
