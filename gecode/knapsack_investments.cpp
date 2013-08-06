/*  
  Knapsack (investment) problem  in Gecode.

  Problem from the swedish book
  Lundgren, Rönnqvist, Värbrand "Optimeringslära", page 393ff.
  """  
  A company shall invest in some building projects with the following
  limits:

   - budget of 225 Mkr (million Swedish Kronor)
   - 28 persons available
   - maximum 9 projects can be selected
   - some project may not be selected together with other projects, and some
     projects must be selected together with other.
  
  [I'm keeping the swedish object names.]

  No.  Object   Value(kkr) Budget(Mkr) Personell  Not with  Requires
  1  Ishall      600        35            5        10        -
  2  Sporthall   400        34            3        -         -
  3  Hotell      100        26            4        -         15
  4  Restaurang  150        12            2        -         15
  5  Kontor A     80        10            2        6         -
  6  Kontor B    120        18            2        5         -
  7  Skola       200        32            4        -         -
  8  Dagis       220        11            1        -         7
  9  Lager        90        10            1        -         -
  10 Simhall     380        22            5        1         -
  11 Hyreshus    290        27            3        15        -
  12 Bilverkstad 130        18            2        -         -
  13 Tennishall   80        16            2        -         2
  14 Idrottsanl. 270        29            4        -         2
  15 Båthamn     280        22            3        11        -
  """  

  Solution (page 395): 
  The following project is selected
    1,2,4,6,7,8,12,14,15
  and optimal value is 2370kkr.


  Here I use a more general model than the book's model.

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/knapsack_investments.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/knapsack_investments.pl
  * ECLiPSe: http://www.hakank.org/eclipse/knapsack_investments.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class Investments : public MaximizeScript {
protected:
  
  static const int num_projects = 15;
  static const int max_budget = 225; 
  static const int max_projects = 9; 
  static const int max_persons = 28; 

  IntVarArray x; // the choosen projects
  IntVar total_persons;
  IntVar total_budget; 
  IntVar total_projects;

  IntVar total_values; // the objective to maximize


public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  Investments(const SizeOptions& opt) 
  :    
    x(*this, num_projects, 0, 1),
    total_persons(*this, 0, max_persons),
    total_budget(*this, 0, max_budget),
    total_projects(*this, 0, max_budget),
    total_values(*this, 0, 99999) // large number
  {

    int _values[] = {600,400,100,150, 80,120,200,220, 90,380,290,130, 80,270,280};
    IntArgs values(num_projects, _values);

    int _budgets[] = {35,34,26,12,10,18,32,11,10,22,27,18,16,29,22};
    IntArgs budgets(num_projects, _budgets);

    int _personell[] = {5,3,4,2,2,2,4,1,1,5,3,2,2,4,3};
    IntArgs personell(num_projects, _personell);


    int num_requires = 5;
    // 1-based
    int _requires[] = {
      3, 15,
      4, 15,
      8, 7,
      13, 2,
      14, 2
    };
    IntArgs requires(num_requires*2, _requires);

    int num_not_with = 6;
    // 1-based
    int _not_with[] = 
      {
        1, 10,
        5, 6,
        6, 5,
        10, 1,
        11, 15,
        15, 11
      };
    IntArgs not_with(num_not_with*2, _not_with); 

    linear(*this, personell, x, IRT_EQ, total_persons);
    linear(*this, budgets, x, IRT_EQ, total_budget);
    rel(*this, total_projects==sum(x)); 
    linear(*this, values, x, IRT_EQ, total_values);
    
    // show all optimal solutions (there is exactly one)
    if (opt.search() == SEARCH_DFS) {
      rel(*this, total_values >= 2370);
    }

    //
    // resource limits:
    //

    // total_budget <= max_budget
    rel(*this, total_budget <= max_budget);

    // total_persons <= max_persons
    rel(*this, total_persons <= max_persons);

    // total_projects <= max_projects
    rel(*this, total_projects <= max_projects);


    /*
      Special requirements, using standard Integer Programming "tricks"
    */

    // projects that require other projects
    for(int i = 0; i < num_requires; i++) {
      int r1 = requires[i*2+0] - 1;
      int r2 = requires[i*2+1] - 1;
      rel(*this, x[r1] - x[r2] <= 0);
    }

    // projects excluding other projects
    for(int i = 0; i < num_not_with; i++) {
      int n1 = not_with[i*2+0]-1;
      int n2 = not_with[i*2+1]-1;
      rel(*this, x[n1] + x[n2] <= 1);
    }


    //
    // Branching.
    //
    branch(*this, x, INT_VAR_MAX_MAX(), INT_VAL_MAX()); 


  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "total_values: " << total_values << endl;
    os << "total_persons: " << total_persons << endl;
    os << "total_budget: " << total_budget << endl;
    os << "total_projects: " << total_projects << endl;
    os << "x: " << x << endl;
    os << "The following projects should be selected (1-based)" << endl;
    for(int i = 0; i < num_projects; i++) {
      if (x[i].val() == 1) {
        os << i+1 << " ";
      }
    }
    os << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Investments(bool share, Investments& s) : MaximizeScript(share,s) {
    x.update(*this, share, s.x);
    total_persons.update(*this, share, s.total_persons);
    total_budget.update(*this, share, s.total_budget);
    total_projects.update(*this, share, s.total_projects);
    total_values.update(*this, share, s.total_values);
  }

  // Return cost
  virtual IntVar cost(void) const {
    return total_values;
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Investments(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Investments");
  opt.solutions(0);

  opt.search(Investments::SEARCH_BAB);
  opt.search(Investments::SEARCH_DFS, "dfs");
  opt.search(Investments::SEARCH_BAB, "bab");
 
  opt.parse(argc,argv);

  switch (opt.search()) {
    case Investments::SEARCH_DFS:
      MaximizeScript::run<Investments,DFS,SizeOptions>(opt); break;
    case Investments::SEARCH_BAB:
      MaximizeScript::run<Investments,BAB,SizeOptions>(opt); break;
    }

  return 0;

}


