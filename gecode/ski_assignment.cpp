/*

  Ski assignment problem in Gecode.

  From  
  Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008, Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  """
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
  decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
  They'll let you ski all winter for free in exchange for helping their ski rental 
  shop with an algorithm to assign skis to skiers. Ideally, each skier should 
  obtain a pair of skis whose height matches his or her own height exactly. 
  Unfortunately, this is generally not possible. We define the disparity between 
  a skier and his or her skis to be the absolute value of the difference between 
  the height of the skier and the pair of skis. Our objective is to find an 
  assignment of skis to skiers that minimizes the sum of the disparities. 
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
  following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.

  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/ski_assignment.mzn
  * Comet: http://www.hakank.org/comet/ski_assignment.co
  * ECLiPSe: http://www.hakank.org/eclipse/ski_assignment.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/ski_assignment.pl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class SkiAssignment : public MinimizeScript {
protected:

  static const int num_skis = 6;
  static const int num_skiers = 5;

  IntVarArray x;
  IntVar z; // the differences, to minimize

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB
  };

  // Actual model
  SkiAssignment(const SizeOptions& opt) : 
    x(*this, num_skiers, 0, num_skis),
    z(*this, 0, 100)
  {

    int _ski_heights[] = {1, 2, 5, 7, 13, 21};
    IntArgs ski_heights(num_skis, _ski_heights);

    int _skier_heights[] = {3, 4, 7, 11, 18};
    IntArgs skier_heights(num_skiers, _skier_heights);

    distinct(*this, x);
    
    // var int: z = sum(i in 1..num_skiers) ( abs(ski_heights[x[i]] - skier_heights[i]) );
    IntVarArgs tmp;
    for(int i = 0; i < num_skiers; i++) {
      tmp << expr(*this, abs(element(ski_heights, x[i]) - skier_heights[i]));
    }
    rel(*this, z == sum(tmp));

    // all optimal solutions if the program is started with
    //    -search dfs
    if (opt.search() == SEARCH_DFS) {
      rel(*this, z == 7, opt.icl());
    }

    branch(*this, x, INT_VAR_SIZE_MAX(), INT_VAL_MIN());   
  }

  // Constructor for cloning s
  SkiAssignment(bool share, SkiAssignment& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SkiAssignment(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x    : " << x << std::endl;
    os << "z    : " << z << std::endl;
    os << std::endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return z;
  }


};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("SkiAssignment");
  opt.solutions(0);

  opt.search(SkiAssignment::SEARCH_BAB);
  opt.search(SkiAssignment::SEARCH_DFS, "dfs");
  opt.search(SkiAssignment::SEARCH_BAB, "bab");


  if (!opt.size()) 
    opt.size(4);

  opt.parse(argc,argv);

  if (opt.search() == SkiAssignment::SEARCH_DFS) {
    Script::run<SkiAssignment, DFS, SizeOptions>(opt);
  } else {
    MinimizeScript::run<SkiAssignment, BAB, SizeOptions>(opt);
  }


  return 0;
}


