/*

  Global constraint alldifferent_on_intersection in Gecode.

  This model implements a decomposition of the global constraint
  alldifferent_on_intersection.

% From Global Constraint Catalogue
% http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_on_intersection.html
% """
% The values that both occur in the VARIABLES1 and VARIABLES2 collections 
% have only one occurrence.
% 
% Example
% (
%  <5, 9, 1, 5>,
%  <2, 1, 6, 9, 6, 2>
% )
% 
% The alldifferent_on_intersection constraint holds since the values 9 and 1 
% that both occur in <5, 9, 1, 5> as well as in <2, 1, 6, 9, 6, 2> have 
% exactly one occurrence in each collection.
% """

  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/alldifferent_on_intersection.mzn
  - SICStus Prolog: http://www.hakank.org/sicstus/alldifferent_on_intersection.pl
  - ECLiPSe: http://www.hakank.org/eclipse/alldifferent_on_intersection.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void alldifferent_on_intersection(Space& space, 
                                  IntVarArray x, 
                                  int m,
                                  IntVarArray y, 
                                  int n, 
                                  IntConLevel icl = ICL_BND) {

  for(int i = 0; i < m; i++) {
    BoolVarArgs tmp;
    for(int j = 0; j < n; j++) {
      tmp << expr(space, x[i] == y[j]);
    }
    rel(space, sum(tmp) <= 1);
  }


  for(int j = 0; j < n; j++) {
    BoolVarArgs tmp;
    for(int i = 0; i < m; i++) {
      tmp << expr(space, x[i] == y[j]);
    }
    rel(space, sum(tmp) <= 1);
  }


}

class AlldifferentOnIntersection : public Script {
protected:

  static const int m = 4;
  static const int n = 6;

  IntVarArray x;
  IntVarArray y;

public:

  // Actual model
  AlldifferentOnIntersection(const SizeOptions& opt) : 
    x(*this, m, 1, 9),
    y(*this, n, 1, 9)
  {

    int _xtmp[] = {5,9,1,5};
    IntArgs xtmp(m, _xtmp);
    for(int i = 0; i < m; i++) {
      rel(*this, x[i] == xtmp[i]);
    }

    int _ytmp[] = {2, 1, 6, 9, 6, 2};
    IntArgs ytmp(n, _ytmp);
    for(int j = 0; j < n; j++) {
      rel(*this, y[j] == ytmp[j]);
    }

    alldifferent_on_intersection(*this, x, m, y, n, opt.icl());

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, y, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  AlldifferentOnIntersection(bool share, AlldifferentOnIntersection& s) : Script(share,s) {
    x.update(*this, share, s.x);
    y.update(*this, share, s.y);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AlldifferentOnIntersection(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << "y: " << y << std::endl;
    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("AlldifferentOnIntersection");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<AlldifferentOnIntersection,DFS,SizeOptions>(opt);

  return 0;
}


