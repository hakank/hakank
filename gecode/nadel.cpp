/*
  
  Nadel's construction problem in Gecode.
  

  From Rina Dechter "Constraint Processing", page 5.
  Attributes the problem to
  B.A. Nadel "Constraint satisfaction algorithms" (1989).
  """
  * The recreation area should be near the lake.
  
  * Steep slopes are to be avoided for all but the recreation area.
  * Poor soil should be avoided for those developments that 
    involve construction, namely the apartments and the family houses.
  
  * The highway, being noisy, should not be near the apartments, 
    the housing, or the recreation area.
  
  * The dumpsite should not be visible from the apartments, 
    the houses, or the lake.
  
  * Lots 3 and 4 have bad soil.
  * Lots 3, 4, 7, and 8 are on steep slopes .
  * Lots 2, 3, and 4 are near the lake.
  * Lots 1 and 2 are near the highway.
  """

  Comment: 
  I have not found any solution that satisfies all the constraints.
  However this "soft" version counts the broken constraints
  and minimizes to 1 broken constraint.
  
  The model (which - of course - could be wrong) generates 28 different 
  solutions with 1 broken constraints. The broken constraints are either
    - steep_slopes constraints or
    - near_dump constraints.
 

  Compare with the following model: 
  * MiniZinc: http://www.hakank.org/minizinc/nadel.mzn


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


//
// Utility function for checking the constraints with reification
//
//  checks that x[y] = val, refied with reif.
//
void check_c(Space &space, IntArgs x, IntVar y, int val, BoolVar reif, IntConLevel icl = ICL_DOM) {

  BoolVar b(space, 0, 1);
  element(space, x, y, b, icl);
  rel(space, b, IRT_EQ, val, reif, icl);
}

/**
 *
 * "Matrix" variant of the element constraint with reification
 *
 * where m is an IntVarArray interpreted as a matrix 
 *
 *   m[x*n + y] = val         (which should be interpreted as m'[x, y] = val)
 *
 * m: the IntVarArray array
 * x, y: two IntVars
 * rows: row dimension
 * cols: col dimension
 * val: integer value to set
 * reif: reification of val = m[x*row +y]
 * icl: consistency levels 
 *
 */
void element_m2(Space & space, IntArgs m, IntVar x, IntVar y, int rows, int cols, int val, BoolVar reif, IntConLevel icl = ICL_DOM) {

  IntVar x_n(space, 0, (rows*cols)-1);
  IntVar ix(space, 0, (rows*cols)-1); 
  IntVar e1(space, 0, rows-1);
  rel(space, x_n == x*rows, icl);         // x_n = x*rows
  rel(space,  ix == x_n+y, icl);          // ix = x*row + y
  element(space, m, ix, e1, icl);          // e1 = m[x*row +y]
  rel(space, e1, IRT_EQ, val, reif, icl);  // val = m[x*row +y] (reified)

} // end element_m2




class Nadel : public MinimizeScript {
protected:

  static const int n = 8;  // number of lots
  static const int d = 5;  // number of developments
  static const int c = 13; // number of constraints

  BoolVarArray broken;    // indicator of the broken constraints
  IntVar total_count;     // number of broken constraints (to minimize)

  // the development to place in one of the lots
  IntVarArray developments;

  // number of total broken constraints (from command line)
  int total_count_args;  

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  };

  Nadel(const SizeOptions& opt) 
    : 
    broken(*this, c, 0, 1),
    total_count(*this, 0, c),
    developments(*this, d, 0, n),
    total_count_args(opt.size())
  {

    // the developments
    IntVar 
      recreation(developments[0]), 
      apartments(developments[1]), 
      houses(developments[2]), 
      cemetery(developments[3]), 
      dump(developments[4]);


    /**
     * Lots 3 and 4 have bad soil.
     * Lots 3, 4, 7, and 8 are on steep slopes .
     * Lots 2, 3, and 4 are near the lake.
     * Lots 1 and 2 are near the highway.
     *
     */

    //                      1, 2, 3, 4, 5, 6, 7, 8
    int _bad_soil[]     =  {0, 0, 1, 1, 0, 0, 0, 0};
    int _steep_slopes[] =  {0, 0, 1, 1, 0, 0, 1, 1};
    int _near_lake[]    =  {0, 1, 1, 1, 0, 0, 0, 0};
    int _near_highway[] =  {1, 1, 0, 0, 0, 0, 0, 0};
    
    IntArgs bad_soil(n, _bad_soil); 
    IntArgs steep_slopes(n, _steep_slopes); 
    IntArgs near_lake(n, _near_lake); 
    IntArgs near_highway(n, _near_highway); 

    //
    // proximity of lots
    // neighborhood matrix for the dump placement
    //
    int _near_lots[] = 
      {
        // 1  2  3  4  5  6  7  8  
           0, 1, 0, 0, 1, 0, 0, 0, // 1
           1, 0, 1, 0, 0, 1, 0, 0, // 2 
           0, 1, 0, 1, 0, 0, 1, 0, // 3 
           0, 0, 1, 0, 0, 0, 0, 1, // 4
           1, 0, 0, 0, 0, 1, 0, 0, // 5
           0, 1, 0, 0, 1, 0, 1, 0, // 6
           0, 0, 1, 0, 0, 1, 0, 1, // 7
           0, 0, 0, 1, 0, 0, 1, 0, // 8
      };

    IntArgs near_lots(n*n, _near_lots);

    if (total_count_args) {
      rel(*this, total_count == total_count_args, opt.icl()); // for all solutions
    }

    distinct(*this, developments, opt.icl());

    // number of broken constraints
    linear(*this, broken, IRT_EQ, total_count, opt.icl());


    /**
     * The constraints.
     * Note: Compared to the MiniZinc model we reverse the values
     *      in the constraint checks so we can still minimize 
     *      the number of broken constraints. Another approach
     *       is to maximize the number of not broken constraints,
     *       but that don't feel that right.
     */

    // * The recreation area should be near the lake.
    //    Note: the value should be 1 for not broken (see above)
    check_c(*this, near_lake, recreation   , 0, broken[0], opt.icl());

    // * Steep slopes are to be avoided for all but the recreation area.
    //    Note: For these and the following constraints,
    //          the value should be 0 for not broken (see above)
    check_c(*this, steep_slopes, apartments, 1, broken[1], opt.icl());
    check_c(*this, steep_slopes, houses    , 1, broken[2], opt.icl());
    check_c(*this, steep_slopes, cemetery  , 1, broken[3], opt.icl());
    check_c(*this, steep_slopes, dump      , 1, broken[4], opt.icl());


    // * Poor soil should be avoided for those developments that 
    //   involve construction, namely the apartments and the family houses.
    check_c(*this, bad_soil, apartments    , 1, broken[5], opt.icl());
    check_c(*this, bad_soil, houses        , 1, broken[6], opt.icl());


    // * The highway, being noisy, should not be near the apartments, 
    //   the housing, or the recreation area.
    check_c(*this, near_highway, apartments, 1, broken[7], opt.icl());
    check_c(*this, near_highway, houses    , 1, broken[8], opt.icl());
    check_c(*this, near_highway, recreation, 1, broken[9], opt.icl());

    // * The dumpsite should not be visible from the apartments, 
    //   the houses, or the lake.
    // not near the lake
    check_c(*this, near_lake   , dump      , 1, broken[10], opt.icl());

    // not near the house 
    BoolVar b11(*this, 0, 1);
    element_m2(*this, near_lots, dump, houses, n, n, 1, b11, opt.icl());
    element_m2(*this, near_lots, houses, dump, n, n, 1, b11, opt.icl());
    rel(*this, b11, IRT_EQ, 1, broken[11], opt.icl());

    // not near the apartments  
    BoolVar b12(*this, 0, 1);
    element_m2(*this, near_lots, dump, apartments, n, n, 1, b12, opt.icl());
    element_m2(*this, near_lots, apartments, dump, n, n, 1, b12, opt.icl());
    rel(*this, b12, IRT_EQ, 1, broken[12], opt.icl());

    // branching
    branch(*this, broken, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, developments, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "#broken: " << total_count << " broken: " << broken << std::endl;
    os << "Developments: " << developments << std::endl;
    os << "The broken constraint(s): ";
    std::string la("near lake");
    std::string st("steep slopes");
    std::string so("bad soil");
    std::string hi("near highway");
    std::string du("near dump");
    std::string constraints[] = {la,st,st,st,st,so,so,hi,hi,hi,du,du,du};
    for(int i = 0; i < c; i++) {
      if (broken[i].val()) {
        os << constraints[i] << " (" << i << ") ";
        }
    }
    os << std::endl;
    os << std::endl;
  }


  // Constructor for cloning s
  Nadel(bool share, Nadel& s) : MinimizeScript(share,s) {
    broken.update(*this, share, s.broken);
    total_count.update(*this, share, s.total_count);
    developments.update(*this, share, s.developments);
  }

  // cost to minimize
  // Return cost
  virtual IntVar cost(void) const {
    return total_count;
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Nadel(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("Nadel");

  opt.solutions(0);
  opt.icl(ICL_BND);
  opt.search(Nadel::SEARCH_BAB);
  opt.search(Nadel::SEARCH_BAB, "bab");
  opt.search(Nadel::SEARCH_DFS, "dfs");

  opt.parse(argc,argv);
  if (opt.size()) {
    opt.search(Nadel::SEARCH_DFS);
  }

  if (opt.search() == Nadel::SEARCH_DFS) {
    Script::run<Nadel,DFS,SizeOptions>(opt);
  } else {
    MinimizeScript::run<Nadel,BAB,SizeOptions>(opt);
  }

  return 0;
}


