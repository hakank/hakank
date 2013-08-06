/*

  Implementation of de Bruijn sequences in Gecode.

  This model is discussed in the blog post
  "de Bruijn sequences in Gecode (and other systems)"
  http://www.hakank.org/constraint_programming_blog/2009/03/de_bruijn_sequences_in_gecode.html


  This model can be used for both 
  * classic sequences, i.e. the sequence length is base^n 
    (n is the number of bits),
  * and "arbitrary", where the sequence length is arbitrary.

  The basic principle of the model is the following.

  Given:
     - a base
     - number of bits (n)
     - length of sequence (m)

  Then:
   - make a list of (distinct) integers in the range 0..(base^n)-1.
     These are the nodes in a de Bruijn graph.

   - calculate the "bit sequence" (in base) for each integer.
     this is a matrix with m rows and n columns (called binary)

   - apply the de Bruijn condition for each consecutive integer, i.e.
     the first elements in binary[r] is the same as the last elements 
     in binary[r-1], and also "around the corner"

   - the de Bruijn sequence is then the first element in each row,
     here called bin_code.

  For "classic" de Bruin sequences this is overkill, but it makes
  it possible to calculate sequences of arbitrary lengths.

  For more about de Bruijn sequences see 
  http://en.wikipedia.org/wiki/De_Bruijn_sequence


  Compare with the following web based programs which also explains the 
  principles:
  * http://www.hakank.org/comb/debruijn.cgi      
    classic version
  * http://www.hakank.org/comb/debruijn_arb.cgi
    "arbitrary" version, using a different approach


  Also compare with the following constraint programming models using the 
  same principle as this Gecode model:
  * MiniZinc: http://www.hakank.org/minizinc/debruijn_binary.mzn
  * Comet   : http://www.hakank.org/comet/debruijn.co
  * Choco   : http://www.hakank.org/choco/DeBruijn.java
  * JaCoP   : http://www.hakank.org/JaCoP/DeBruijn.java
  * Gecode/R: http://www.hakank.org/gecode_r/debruijn_binary.rb



  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

//
// The options for this model.
//
// Run the program with the -help option to see all options.
//
// Note: It seems that these options must be after all
//       of the builtin options from the Options class.
// 
class DeBruijnOptions : public Options {

private:

  // Option-handling members
  Driver::UnsignedIntOption _base_option;         // -base: base to use
  Driver::UnsignedIntOption _n_option;            // -n: number of "bits"
  Driver::UnsignedIntOption _m_option;            // -m: length of sequence
  /*
  Driver::StringOption _int_var_option;           // -int-var: IntVarBranch
  Driver::StringOption _int_val_option;           // -int-val: IntValBranch
  */
  Driver::UnsignedIntOption _print_matrix_option; // -print-matrix: print the matrix
  Driver::UnsignedIntOption _print_x_option;      // -print-x: print the x (integers)
  Driver::UnsignedIntOption _same_occurrences_option; // -same-occurrences: same occurrences 
                                              //  in the de Bruijn sequence

public:

  // Initialize options for example with name 
  DeBruijnOptions(const char* n)
    : Options(n),
      _base_option("-base",
                   "base to use",
                   2),
      _n_option("-n",
                "number of bits to use.",
                3),
      _m_option("-m",
                "length of the sequence."
                ),
      /*
      _int_var_option("-int-var",
                      "options for IntVarBranch",
                      INT_VAR_MIN_MIN),
      _int_val_option("-int-val",
                      "options for IntValBranch",
                      INT_VAL_MIN),
      */
      _print_matrix_option("-print-matrix",
                           "1 prints the binary matrix.",
                           0),
      _print_x_option("-print-x",
                      "1 prints x (array of integers).",
                      0),
      _same_occurrences_option("-same-occurrences",
                      "1: all occurrences of element in the the de Bruijn sequence (bin_code) should be the same.",
                      0)
  {
    add(_base_option);
    add(_n_option);
    add(_m_option);
    add(_print_matrix_option);
    add(_print_x_option);
    add(_same_occurrences_option);

    //
    // The names for these branching options are from the Gecode/FlatZinc mapping 
    // of the the MiniZinc branch options.
    //

    // IntVarBranch
    /*
    add(_int_var_option);
    _int_var_option.add(INT_VAR_NONE          , "input-order", "use VAR_NONE");
    _int_var_option.add(INT_VAR_SIZE_MIN      , "first-fail", "use VAR_SIZE_MIN");
    _int_var_option.add(INT_VAR_SIZE_MAX      , "anti-first-fail", "use VAR_SIZE_MAX");
    _int_var_option.add(INT_VAR_MIN_MIN       , "smallest", "use VAR_MIN_MIN");
    _int_var_option.add(INT_VAR_MAX_MAX       , "largest", "use VAR_MAX_MAX");
    _int_var_option.add(INT_VAR_DEGREE_MAX    , "occurrence", "use VAR_DEGREE_MAX");
    _int_var_option.add(INT_VAR_REGRET_MIN_MAX, "max-regret", "use VAR_REGRET_MIN_MAX");

    // IntValBranch
    add(_int_val_option);
    _int_val_option.add(INT_VAL_MIN           , "indomain-min", "use VAL_MIN");
    _int_val_option.add(INT_VAL_MAX           , "indomain-max", "use VAL_MAX");
    _int_val_option.add(INT_VAL_MED           , "indomain-median", "use VAL_MED");
    _int_val_option.add(INT_VAL_SPLIT_MIN     , "indomain-split", "use VAL_SPLIT_MIN");
    */

  }

  // Parse options from arguments argv (number is argc)
  void parse(int& argc, char* argv[]) {

    // Parse regular options
    Options::parse(argc,argv);

  }

  // Return base to use
  unsigned int base(void) const  { return _base_option.value();  }

  // Return number of bits
  unsigned int n(void) const { return _n_option.value(); }

  // Return length of sequence
  unsigned int m(void) const { 

    // if no -m, default to m = base^n
    if (!_m_option.value()) {
      int mm = pow(_base_option.value(), _n_option.value());
      return mm;
    } else {
      return _m_option.value(); 
    }

  }

  // Return if the matrix should be printed
  unsigned int print_matrix(void) const { return _print_matrix_option.value(); }

  // Return if x (array of integers) should be printed
  unsigned int print_x(void) const { return _print_x_option.value(); }

  // Return if all elements in the de Bruijn sequence should be the same.
  // Only applicable if m % base == 0.
  unsigned int same_occurrences(void) const { return _same_occurrences_option.value(); }

  /*
  // Return IntVarBranch 
  unsigned int int_var(void) const { return _int_var_option.value(); }

  // Return IntValBranch 
  unsigned int int_val(void) const { return _int_val_option.value(); }
  */

};


//
// the de Bruijn model
//
class DeBruijn : public Script {
protected:

  //
  // Note: The names base, n, and, m are perhaps unfortunate, but are kept here
  //       since they are used in my other implementations (see above).
  //
  const int base;             // base: the base to use, 
                              //       i.e. the alphabet 0..base-1

  const int n;                // n: number of bits to use 
                              //    E.g. 
                              //     if base = 2 and n = 4 then we use 
                              //     the integers 0..base^n-1 = 0..2^4 -1, 
                              //     i.e. 0..15.

  const int m;                // m: the length of the sequence.
                              //    Default is the "classic" de Bruijn sequence, 
                              //    i.e. base^n. 

  IntVarArray x;              // x: integers (distinct) 
                              //    of range 0...(base^n)-1

  IntVarArray binary;         // binary: the "binary" representation of the numbers
                              // binary_mat is a matrix with m rows and n columns

  IntVarArray bin_code;       // bin_code: the de Bruijn sequence in 
                              //           base-representation

  const int print_matrix;     // 1 if printing the binary matrix
  const int print_x;          // 1 if printing x (integer array)

  /*
  const int int_var;          // branching for IntVarBranch
  const int int_val;          // branching for IntValBranch
  */

  const int same_occurrences; // 1 if requiring that all elements in 
                              // the de Bruijn sequence should be of same
                              // occurrences
  IntVarArray gcc;            // number of occurrences of the values in the 
                              // de Bruijn sequence

public:


  DeBruijn(const DeBruijnOptions& opt) 
  : 
    base(opt.base()),
    n(opt.n()),
    m(opt.m()),
    x(*this, m, 0, pow(base,n)-1), 
    binary(*this, m*n,  0, base-1),
    bin_code(*this, m,  0, base-1),
    print_matrix(opt.print_matrix()),
    print_x(opt.print_x()),
    // int_var(opt.int_var()),
    // int_val(opt.int_var()),
    same_occurrences(opt.same_occurrences()),
    gcc(*this, base, 0, m)
  {

    std::cout << "base: " << base << std::endl;
    std::cout << "number of bits (n): "    << n    << std::endl;
    std::cout << "length of sequence (m): "    << m    << std::endl;
    // std::cout << "print_matrix: "    << print_matrix  << std::endl;
    std::cout << "same occurrences: " << same_occurrences << std:: endl;
    std::cout << std::endl;

    //
    // Matrix version of binary. 
    // Note: the order is columns (n), rows (m)
    //
    Matrix<IntVarArray> binary_mat(binary, n, m);

    //
    // All integers must be distinct.
    //
    distinct(*this, x, opt.icl());
    

    //
    // symmetry breaking: the minimum element should be the first element
    //
    min(*this, x, x[0], opt.icl());

    //
    // channel x[row] <-> binary[row...]
    //
    // coefficients for the channelling
    IntArgs coeffs(n);
    for(int r = 0; r < n; r++) {
      coeffs[r] = pow(base, n-r-1);
    }
    for(int r = 0; r < m; r++) {
      linear(*this, coeffs, binary_mat.row(r), IRT_EQ, x[r], opt.icl());
    }

    //
    // The de Bruijn condition: 
    // the first elements in binary[i] is the same as the last elements 
    // in binary[i-1]
    //
    for(int r = 1; r < m; r++) {
      for(int c = 1; c < n; c++) {
        rel(*this, binary_mat(c-1, r), IRT_EQ, binary_mat(c, r-1), opt.icl());
      }
    }

    // ... and around the corner
    for(int c = 1; c < n; c++) {
      rel(*this, binary_mat(c, m-1), IRT_EQ, binary_mat(c-1, 0), opt.icl() );
    }

    //  
    // converts binary -> bin_code, i.e. the first element in each row .
    // bin_code is the de Bruijn sequence.
    //
    for(int r = 0; r < m; r++) 
      rel(*this, binary_mat(0, r), IRT_EQ, bin_code[r], opt.icl());
 

    count(*this, bin_code, gcc, opt.icl());

    // Constrain that there should be  equal number of 
    // occurrences of the elements in the de Bruijn sequence (bin_code).
    if (same_occurrences) {
      int the_count = m / base;  // number of occurrences of each element
      rel(*this, gcc[0], IRT_EQ, the_count, opt.icl());
      for(int i = 1; i < base; i++) {
        rel(*this, gcc[i], IRT_EQ, gcc[i-1], opt.icl());
      }
    }

    

    /*
    branch(*this, x       , 
           static_cast<IntVarBranch>(opt.int_var()), 
           static_cast<IntValBranch>(opt.int_val())); 
    branch(*this, binary, 
           static_cast<IntVarBranch>(opt.int_var()), 
           static_cast<IntValBranch>(opt.int_val())); 
    branch(*this, bin_code, 
           static_cast<IntVarBranch>(opt.int_var()), 
           static_cast<IntValBranch>(opt.int_val())); 

    branch(*this, gcc, 
             static_cast<IntVarBranch>(opt.int_var()), 
             static_cast<IntValBranch>(opt.int_val())); 
    */

    branch(*this, x, 
           INT_VAR_SIZE_MIN(), 
           INT_VAL_MIN()); 
    branch(*this, binary, 
           INT_VAR_SIZE_MIN(), 
           INT_VAL_MIN()); 
    branch(*this, bin_code, 
           INT_VAR_SIZE_MIN(), 
           INT_VAL_MIN()); 
    branch(*this, gcc, 
           INT_VAR_SIZE_MIN(), 
           INT_VAL_MIN()); 


  }

  //
  // Print solution
  //
  virtual void
  print(std::ostream& os) const {
    if (print_x) {
      os << "x:" << x << std::endl;
    }
    os << "de Bruijn sequence: " << bin_code << std::endl;
    os << "gcc: " << gcc << std::endl;
    if (print_matrix == 1) {
       // Note:  columns, rows
       Matrix<IntVarArray> binary_mat(binary, n, m);
       os << "binary matrix: " << std::endl;
       for(int r = 0; r < m; r++) {
        for(int c = 0; c < n; c++) {      
          os << binary_mat(c, r) << " ";
        }
        os << std::endl;
       }
     }
    os << std::endl;

  }

  //
  // Constructor for cloning s
  //
  DeBruijn(bool share, DeBruijn& s) : Script(share,s),
                                      base(s.base),
                                      n(s.n), 
                                      m(s.m),
                                      print_matrix(s.print_matrix),
                                      print_x(s.print_x),
                                      // int_var(s.int_var),
                                      // int_val(s.int_val),
                                      same_occurrences(s.same_occurrences),
                                      gcc(s.gcc)
  {
    x.update(*this, share, s.x);
    binary.update(*this, share, s.binary);
    bin_code.update(*this, share, s.bin_code);
    // if (same_occurrences) {
    gcc.update(*this, share, s.gcc);
    //}
  }

  //
  // Copy during cloning
  //
  virtual Space*
  copy(bool share) {
    return new DeBruijn(share, *this);
  }

};


//
// main
//
int
main(int argc, char* argv[]) {
  DeBruijnOptions opt("DeBruijn");
  opt.solutions(1);
  opt.iterations(20000);
  opt.icl(ICL_BND);

  opt.parse(argc,argv);

  unsigned int pow_base_n = pow(opt.base(), opt.n());
  if (opt.m() > pow_base_n) {
    std::cout << "Error: length of sequence (m)  must be <= base^n (" << pow(opt.base(), opt.n()) << ")" << std::endl;
    return 1;
  }

  if (opt.same_occurrences() && (opt.m() % opt.base() != 0)) {
    std::cout << "Error: The option '-same-occurrences' requires that m % base == 0." << std::endl;
    return 1;
  }

  Script::run<DeBruijn,DFS,DeBruijnOptions>(opt);
  return 0;
}


