/*

  Generating all solutions for a regular expression in Gecode.

  Regular expressions in Gecode is described here:
  http://www.gecode.org/gecode-doc-latest/classGecode_1_1REG.html


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <string>
#include <vector>

using namespace Gecode;

// all solutions
std::vector<std::string> all_solutions; 

class AllRegexp : public Script {
protected:

  const int size;     // size of problem
  IntVarArray X;   // the array
  const int model; // the choosen model

public:

  enum models {
    MODEL_MANKELL,
    MODEL_KJELLERSTRAND
  };

  enum {
    aa,ab,ac,ad,ae,af,ag,
    ah,ai,aj,ak,al,am,an,
    ao,ap,aq,ar,as,at,au,
    av,aw,ax,ay,az,
    aaring,aauml,aouml,
    aspace
  };

  AllRegexp(const SizeOptions& opt) 
    : 
    size(opt.size()),
    X(*this, size, 0, aspace),
    model(opt.model())
  {

    std::cout << "size: " << size << std::endl;

    REG 
      a(aa),b(ab),c(ac),d(ad),e(ae),f(af),g(ag),
      h(ah),i(ai),j(aj),k(ak),l(al),m(am),n(an),
      o(ao),p(ap),q(aq),r(ar),s(as),t(at),u(au),
      w(aw),x(ax),y(ay),z(az),
      // å           ä            ö
      aring(aaring),auml(aauml), ouml(aouml);


    if (model == MODEL_MANKELL) {

      /**
       *
       * The Henning Mankell problem:
       *
       * See my (swedish) blog post for a background
       * 'Programspråket Icon och stavningen av "Henning Mankell"'
       * http://www.hakank.org/webblogg/archives/000650.html
       *
       * which was based on the problem of Henning Mankell's name
       * Language Blog: 'The mysteries of... what's his name?'
       * http://itre.cis.upenn.edu/~myl/languagelog/archives/000795.html
       *
       * This is the regular expression for Henning Mankell that
       * was explored:
       *
       *   [hm][ea](nk|n|nn)(ing|ell|all)
       *
       *
       * Compare with the Icon program:
       * http://www.hakank.org/unicon/pattern_generation.icn
       *
       */

      extensional(*this, X, 
                  (h|m) +                   // [hm]       
                  (e|a) +                   // 
                  ((n+k) | n | (n+n) ) + 
                  ((i+n+g)|(e+l+l)|(a+l+l))
                  );
      
      

    } else  {

      /**
       *
       * In the comment to the blog post
       * 'Fortsatt stavning av "Henning Mankell". Samt lite om agrep
       * http://www.hakank.org/webblogg/archives/000651.html
       * I generated misspellings of my last name (Kjellerstrand) with 
       * the following regular expression:
       *  k(je|ä)ll(er|ar)?(st|b)r?an?d
       *
       * Also see
       * "Skapa strängar från reguljära uttryck - eller: Tystnaden 
       * de senaste dagarna" [Creating strings from regular expressions - or:
       * The silence of the last days]
       * http://www.hakank.org/webblogg/archives/000652.html
       *
       */
      extensional(*this, X, 
                  k +                  // k
                  (j+e|auml) +         // (je|ä)
                  l+l +                // ll
                  ((e+r)|(a+r))(0,1) + // (er|ar)?
                  ((s + t)|b) +        // (st|b)
                  r(0,1) +             // r?
                  a+                   // a
                  n(0,1) +             // n?
                  d                    // d
                  );

    }

    branch(*this, X, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {

    std::string res; // the full string
    std::string chars = "abcdefghijklmnopqrstuvwxyzåäö";
    for(int i = 0; i < size; i++) {
      // os << str[X[i].val()];
      res.push_back(chars[X[i].val()]);
    }
    os << res << std::endl;

    all_solutions.push_back(res);

  }


  // Constructor for cloning s
  AllRegexp(bool share, AllRegexp& s) : Script(share,s), 
                                        size(s.size),
                                        model(s.model) {
    X.update(*this, share, s.X);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AllRegexp(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("AllRegexp");

  opt.solutions(0);
  opt.model(AllRegexp::MODEL_MANKELL);
  opt.model(AllRegexp::MODEL_MANKELL, "mankell");
  opt.model(AllRegexp::MODEL_KJELLERSTRAND, "kjellerstrand");

  opt.parse(argc,argv);

  // default sizes: for the mankell model
  int min = 6;
  int max = 7;

  if (opt.model() == AllRegexp::MODEL_KJELLERSTRAND) {
    min = 7;
    max = 13;
  }

  // loop through all the different sizes
  for(int s = min; s <= max ; s++) {
    opt.size(s);
    Script::run<AllRegexp,DFS,SizeOptions>(opt);    
  }

  std::cout << "Total: " << all_solutions.size() << " solutions: " << std::endl;
  for(unsigned int i = 0; i < all_solutions.size(); i++) {
    std::cout << all_solutions[i] << std::endl;
  }

  return 0;
}
