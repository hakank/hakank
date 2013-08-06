/*

  Tourist site competition in Gecode.

  From Pierre Flener's presentation 
  "Constraint Technology - A Programming Paradigm on the Rise"
  http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
     pages 5f: problem statement 
     pages 12f: model
     pages 21ff: walktrough of a solution

  With 7 tourist sites and 7 judges:
  """
  Every tourist site is visited by r = 3 judges.
  Every judge visits c = 3 tourist sites.
  Every pair of sites is visited by lambda = 1 common judge.
  """

  There are 151200 solutions to this problem.
  With the additional constraint that Ali should visit Birka, Falun and Lund
  there are 4320 solutions.

  This problem was also presented as "The Airline-of-the-Year Problem"
  in his (Flener's) presentation
  "Constraint Programming - Programming Paradigm on the Rise"
  http://www.it.uu.se/research/group/astra/ATM-CT/Flener.pdf
  page 4f
  The problem is stated as follows for 7 airlines and 7 judges:
  """
  Constant jury: Every airline is tested by 3 judges.
  Constant load: Every judge tests 3 airlines.
  Equity: Every airline pair is tested by 1 common judge.
  """

  Also, see the following models:
  * Comet: http://www.hakank.org/comet/tourist_site_competition.co  
  * MiniZinc: http://www.hakank.org/minizinc/tourist_site_competition.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/tourist_site_competition.pl
  * ECLiPSe: http://www.hakank.org/eclipse/tourist_site_competition.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class TouristSite : public Script {

protected:

  // every site is visited by r judges
  static const int r = 3;
  // every judge visit c sites 
  static const int c = 3;
  // Every pair of sites is visited by exactly lambda common judges
  static const int lambda = 1; 

  static const int num_sites = 7;
  static const int num_judges = 7;

  BoolVarArray x; 
  SetVarArray judges_where; // where are the judges
  SetVarArray sites_who;    // who visits this sites

public:

  // Actual model
  TouristSite(const SizeOptions& opt) : 
    x(*this, num_judges*num_sites, 0, 1),
    judges_where(*this, num_judges, IntSet::empty, IntSet(0, num_sites-1)),
    sites_who(*this, num_sites, IntSet::empty, IntSet(0, num_judges-1))
  {
    
    Matrix<BoolVarArray> x_m(x, num_judges, num_sites);

    // Symmetry breaking: Judge 0 visits site 0,1,2
    for(int s = 0; s < r; s++) {
      rel(*this, x_m(0,s) == 1, opt.icl());
    }

    // Every tourist site is visited by r judges.
    for(int s = 0; s < num_sites; s++) {
      rel(*this, sum(x_m.row(s)) == r, opt.icl());
    }

    // Every judge visits c tourist sites.
    for(int j = 0; j < num_judges; j++) {
      rel(*this, sum(x_m.col(j)) == c, opt.icl());
    }


    // Every pair of sites is visited by lambda common judge.
    for(int s1 = 0; s1 < num_sites; s1++) {
      for(int s2 = 0; s2 < s1; s2++) {
        BoolVarArgs p;
        for(int j = 0; j < num_judges; j++) {
          p << expr(*this, (x_m(j, s1) == 1) && (x_m(j,s2)==1), opt.icl());
        }
        rel(*this, sum(p) == lambda, opt.icl());
      }
    }

    // Where are the judges?
    for(int j = 0; j < num_judges; j++) {
      channel(*this, x_m.col(j), judges_where[j]);
    }

    // Which judges visits the sites?
    for(int s = 0; s < num_sites; s++) {
      channel(*this, x_m.row(s), sites_who[s]);
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  TouristSite(bool share, TouristSite& s) : Script(share,s) {
    x.update(*this, share, s.x);
    judges_where.update(*this, share, s.judges_where);
    sites_who.update(*this, share, s.sites_who);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new TouristSite(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    Matrix<BoolVarArray> m_x(x, num_judges, num_sites);
    // os << "x:\n" << m_x << endl;
    // os << endl;
    os << "judge:  ";
    int c = 7;
    for(int j = 0; j < num_judges; j++) {
      os << j << " ";
      c += 2;
    }
    os << endl;
    while(c-- > 0) {
      os << "-";
    }
    os << endl;
    for(int s = 0; s < num_sites; s++) {
      os << "site " << s << "| ";
      for(int j = 0; j < num_judges; j++) {
        os << m_x(j,s) << " ";
      }
      os << endl;
    }
    os << "\nwhere are the judges:\n";
    for(int j = 0; j < num_judges; j++) {
      os << "judge " << j << ": " << judges_where[j] << endl;
    }
    os << endl;
    os << "who visits each sites::\n";
    for(int s = 0; s < num_sites; s++) {
      os << "site " << s << ": " << sites_who[s] << endl;
    }
    os << endl;

  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("TouristSite");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<TouristSite,DFS,SizeOptions>(opt);

  return 0;
}


