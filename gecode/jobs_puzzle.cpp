/*

  Jobs puzzle in Gecode.
  
  This is a standard problem in Automatic Reasoning.

  From http://www-unix.mcs.anl.gov/~wos/mathproblems/jobs.html
  """
  Jobs Puzzle
  
  There are four people:  Roberta, Thelma, Steve, and Pete.
   Among them, they hold eight different jobs.
   Each holds exactly two jobs.
   The jobs are chef, guard, nurse, clerk, police officer (gender not implied),
   teacher, actor, and boxer.
   The job of nurse is held by a male.
   The husband of the chef is the clerk.
   Roberta is not a boxer.
   Pete has no education past the ninth grade.
   Roberta, the chef, and the police officer went golfing together.

   Question:  Who holds which jobs?
  """

  The answer:
  Chef       Thelma
  Guard      Roberta
  Nurse      Steve
  Clerk      Pete
  Police     Steve
  Teacher    Roberta
  Actor      Pete
  Boxer      Thelma

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/jobs_puzzle.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/jobs_puzzle.pl
  * ECLiPSE: http://www.hakank.org/eclipse/jobs_puzzle.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

using std::cout;
using std::endl;

class Jobs : public Script {
protected:

  IntVarArray jobs;

public:

  // Actual model
  Jobs(const SizeOptions& opt) : 
    jobs(*this, 8, 0, 3)
  {

    enum {
      Roberta,
      Thelma,
      Steve, 
      Pete,
    };

    IntVar 
      chef(jobs[0]), 
      guard(jobs[1]),
      nurse(jobs[2]),
      clerk(jobs[3]),
      police_officer(jobs[4]),
      teacher(jobs[5]),
      actor(jobs[6]),
      boxer(jobs[7]);

    // global_cardinality(Jobs,[2,2,2,2])
    for(int i = 0; i < 4; i++) {
      count(*this, jobs, i, IRT_EQ, 2);
    }

    // The job of nurse is held by a male.
    rel(*this, (nurse == Steve) || (nurse == Pete));

    //  The husband of the chef is the clerk.
    rel(*this, 
        ((clerk == Steve) || (clerk == Pete)) &&
        (chef == Roberta || chef == Thelma) &&
        (chef != clerk));

    //  Roberta is not a boxer.
    rel(*this, Roberta != boxer);

    // Pete has no education past the ninth grade.
    rel(*this, 
        (Pete != teacher) && 
        (Pete != police_officer) && 
        (Pete != nurse));

    // Roberta, [and] the chef, and the police officer went golfing together.
    rel(*this, 
        (Roberta != chef) && 
        (chef != police_officer) && 
        (Roberta != police_officer));
    
    // From the name of the job
    rel(*this, (actor == Steve) || (actor == Pete));


    branch(*this, jobs, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Constructor for cloning s
  Jobs(bool share, Jobs& s) : Script(share,s) {
    jobs.update(*this, share, s.jobs);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Jobs(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "jobs: " << jobs << endl;

    std::string persons_str[] = {"Roberta","Thelma","Steve", "Pete"};
    std::string jobs_str[] = {
      "chef",
      "guard",
      "nurse",
      "clerk",
      "police_officer",
      "teacher",
      "actor",
      "boxer"};

    for(int i = 0; i < 8; i++) {
      cout << jobs_str[i] << ": " << persons_str[jobs[i].val()] << endl;
    }
    cout << endl;
    for(int j = 0; j < 4; j++) {
      cout << persons_str[j] << ":\n";
      for(int i = 0; i < 8; i++) {
        if (jobs[i].val() == j) {
          cout << "\t" << jobs_str[i];
        }
      }
      cout << endl;
    }

    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("Jobs");
  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Jobs,DFS,SizeOptions>(opt);

  return 0;
}

