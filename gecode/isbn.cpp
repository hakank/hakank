/*

  Some explorations of ISBN13 in Gecode.

  See http://en.wikipedia.org/wiki/ISBN


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/isbn.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/isbn.pl
  * Comet: http://www.hakank.org/comet/isbn.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/set.hh>


using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;

//
// The options for this model.
//
class ISBNOptions : public Options {
public:
  // The ISBN to use
  std::string isbn_str;
  unsigned int mult0;
  unsigned int mult1;
  // Initialize options with file name \a s
  ISBNOptions(const char* s)
    : Options(s) {}

  // Parse options from arguments \a argv (number is \a argc)
  void parse(int& argc, char* argv[]) {

    Options::parse(argc,argv);
    isbn_str = "978052112549";
    mult0 = 3;
    mult1 = 1;

    if (argc == 2) {
      isbn_str = argv[1];
    } else if (argc == 3) {
      isbn_str = argv[1];
      mult0 = atoi(argv[2]);
    } else if (argc == 4) {
      isbn_str = argv[1];
      mult0 = atoi(argv[2]);
      mult1 = atoi(argv[3]);
    }


  }

  // Print help message
  virtual void help(void) {
    Options::help();
    std::cerr << "\t(string) (number) (number)" << std::endl
              << "\t\t(ISBN to use) (mult0) (mult1)" << std::endl;
  }
};


class ISBN : public Script {

protected:

  const static int n = 13;

  std::string isbn_str;
  IntVarArray isbn;
  IntVar mult0;
  IntVar mult1;
  
public:

  ISBN(const ISBNOptions& opt) 
    : 
    isbn_str(opt.isbn_str.c_str()),
    isbn(*this, n, 0, 9),
    mult0(*this, 0,9),
    mult1(*this, 0,9)
  {

    int len = isbn_str.size();
    cout << "ISBN to use " << isbn_str << " (len: " << len << ")" << endl;
    
    cout << "opt.mult0: " << opt.mult0 << endl;
    cout << "opt.mult1: " << opt.mult1 << endl;

    if (len == 13) {
      len = 12;
    }

    int _isbn1[n-1]; 
    for(int i = 0; i < len; i++) {
      _isbn1[i] = isbn_str[i]-48;
    }    
    IntArgs isbn1(n-1,_isbn1);
    for(int i = 0; i < len; i++) {
      rel(*this, isbn[i] == isbn1[i]);
    }

    IntVarArgs tmp;
    for(int i = 0; i < n-1; i++) {
      if (i % 2 == 0) {
        tmp << expr(*this, isbn[i]*mult0); // 3
      } else {
        tmp << expr(*this, isbn[i]*mult1); // 1
      }
    }

    /*
      // explicit version using mod()
      IntVar c10(*this, 10,10);
      IntVar mod1(*this, 0,9);
      mod(*this, expr(*this, sum(tmp)), c10, mod1);
      rel(*this, c10-mod1==isbn[n-1]);
    */
    // and with the new syntactic sugar for modulo it's simply:
    rel(*this, isbn[n-1] == 10-(sum(tmp) % 10));

    if (opt.mult0 < 10) {
      rel(*this, mult0 == opt.mult0);
    }

    if (opt.mult1 < 10) {
      rel(*this, mult1 == opt.mult1);
    }

    // branching
    branch(*this, isbn, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, mult0, INT_VAL_MIN());
    branch(*this, mult1, INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "ISBN13: " << isbn << " checkdigit: " << isbn[n-1] << endl;
    os << "mult0: " << mult0 << " mult1: " << mult1 << endl;
    os << endl;
  }


  // Constructor for cloning s
  ISBN(bool share, ISBN& s) : Script(share,s) {
    isbn.update(*this, share, s.isbn);
    mult0.update(*this, share, s.mult0);
    mult1.update(*this, share, s.mult1);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ISBN(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  ISBNOptions opt("ISBN");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<ISBN,DFS,ISBNOptions>(opt);    

  return 0;
}


