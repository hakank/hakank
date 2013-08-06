/*

  Post office problem in Gecode.

  Problem statement:
  http://www-128.ibm.com/developerworks/linux/library/l-glpk2/

  From Winston "Operations Research: Applications and Algorithms":
  """
  A post office requires a different number of full-time employees working
  on different days of the week [summarized below]. Union rules state that
  each full-time employee must work for 5 consecutive days and then receive
  two days off. For example, an employee who works on Monday to Friday
  must be off on Saturday and Sunday. The post office wants to meet its
  daily requirements using only full-time employees. Minimize the number
  of employees that must be hired.

  To summarize the important information about the problem:

    * Every full-time worker works for 5 consecutive days and takes 2 days off
    * Day 1 (Monday): 17 workers needed
    * Day 2 : 13 workers needed
    * Day 3 : 15 workers needed
    * Day 4 : 19 workers needed
    * Day 5 : 14 workers needed
    * Day 6 : 16 workers needed
    * Day 7 (Sunday) : 11 workers needed

  The post office needs to minimize the number of employees it needs
  to hire to meet its demand.
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/post_office_problem2.mzn
  * SICStus: http://www.hakank.org/sicstus/post_office_problem2.pl
  * ECLiPSe: http://www.hakank.org/eclipse/post_office_problem2.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;


class PostOffice : public MinimizeScript {
protected:

  static const int n = 7; // number of days

  IntVarArray x;
  IntVar num_workers;
  IntVar z; // total cost, to minimize

public:

  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  };


  PostOffice(const Options& opt) 
    : 
    x(*this, n, 0, n-1),
    num_workers(*this, 0, 100),
    z(*this, 0, 20000)
  {

    cout << "opt.search(): " << opt.search() << endl;

    int _Need[] = {17, 13, 15, 19, 14, 16, 11};
    IntArgs Need(n, _Need);

    /*
      Total cost for the 5 day schedule.
      Base cost per day is 100.
      Working saturday is 100 extra
      Working sunday is 200 extra.
    */
    int _Cost[] = {500, 600, 800, 800, 800, 800, 700};
    IntArgs Cost(n, _Cost);

    // total cost
    linear(*this, Cost, x, IRT_EQ, z, opt.icl());

    // number of workers
    rel(*this, num_workers == sum(x), opt.icl());

    for(int i = 0; i < n; i++) {
      IntVarArgs tmp;
      for(int j = 0; j < n; j++) {
        if (j != (i+5) % n && j != (i+6) % n) {
          tmp << x[j];
        }
      }
      rel(*this, sum(tmp) >= Need[i], opt.icl());
    }

    if (opt.search() == SEARCH_DFS) {
      rel(*this, z <= 15800);
    }
    

    // branching
    branch(*this, x, INT_VAR_DEGREE_MAX(), INT_VAL_MAX());

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {
    os << "total_cost: " << z << endl;
    os << "num_workers: " << num_workers << endl;
    os << x << endl;
    os << endl;
  }

  // Constructor for cloning s
  PostOffice(bool share, PostOffice& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
    num_workers.update(*this, share, s.num_workers);
  }

  virtual IntVar cost(void) const {
    return z;
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new PostOffice(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("PostOffice");

  opt.solutions(0);

  opt.search(PostOffice::SEARCH_BAB);
  opt.search(PostOffice::SEARCH_BAB, "bab");
  opt.search(PostOffice::SEARCH_DFS, "dfs");

  opt.parse(argc,argv);

  switch (opt.search()) {
    case PostOffice::SEARCH_DFS:
      MinimizeScript::run<PostOffice,DFS,Options>(opt); break;
    case PostOffice::SEARCH_BAB:
      MinimizeScript::run<PostOffice,BAB,Options>(opt); break;
  }

  return 0;
}


