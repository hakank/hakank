/*  
  Broken weights problem in Gecode.
  
  From
  http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first 
  posed by Claude Gaspard Bachet de Méziriac in a book of arithmetic problems 
  published in 1612, and can also be found in Heinrich Dorrie's 100 
  Great Problems of Elementary Mathematics.
  
      A merchant had a forty pound measuring weight that broke 
      into four pieces as the result of a fall. When the pieces were 
      subsequently weighed, it was found that the weight of each piece 
      was a whole number of pounds and that the four pieces could be 
      used to weigh every integral weight between 1 and 40 pounds. What 
      were the weights of the pieces?
  
  Note that since this was a 17th-century merchant, he of course used a 
  balance scale to weigh things. So, for example, he could use a 1-pound 
  weight and a 4-pound weight to weigh a 3-pound object, by placing the 
  3-pound object and 1-pound weight on one side of the scale, and 
  the 4-pound weight on the other side.
  """

  Compare with the following models: 
  - MiniZinc: http://www.hakank.org/minizinc/broken_weights.mzn
  - Gecode: http://www.hakank.org/gecode/coins3.cpp

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;


//
// The options for this model:
//   -num_weights: number of weights (default 4)
//   -orig_weight: the original weight (default 40)
//
class BrokenWeightsOptions : public Options {

private:
  Driver::UnsignedIntOption _num_weights; // number of weights
  Driver::UnsignedIntOption _orig_weight; // original weight

public:
  // Initialize options
  BrokenWeightsOptions(const char* n)
    : Options(n),
      _num_weights("-num_weights",
                   "number of weights",
                   4),
      _orig_weight("-orig_weight",
                   "original weight",
                   40)
  {
    add(_num_weights);
    add(_orig_weight);
  }

  // Parse options from arguments argv (number is argc)
  void parse(int& argc, char* argv[]) {
    Options::parse(argc,argv);
  }

  // Returns num weights
  unsigned int num_weights(void) const { return _num_weights.value(); }
  unsigned int orig_weight(void) const { return _orig_weight.value(); }

};


class BrokenWeights : public MinimizeScript {
protected:

  int num_weights;  // number of different weights, from BrokenWeightOptions
  int orig_weight;  // original weights, from BrokenWeightOptions

  // we will minimize the last element in weights;
  IntVarArray weights;   // the weights
  IntVarArray x;         // the combinations

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB,     // Use branch and bound to optimize
  };

  BrokenWeights(const BrokenWeightsOptions& opt) 
  :    
    num_weights(opt.num_weights()),
    orig_weight(opt.orig_weight()),
    weights(*this, num_weights, 1, orig_weight),
    x(*this, orig_weight*num_weights, -1, 1)
  {

    cout << "Original weight: " << orig_weight << endl;
    cout << "Number of weights: " << num_weights << endl;

    // a matrix version of the different combinations of weights to use
    Matrix<IntVarArgs> x_m(x, num_weights, orig_weight);
    
    // symmetry breaking: order the weights
    rel(*this, weights, IRT_LQ, opt.icl());

    // sum of the weights is w
    linear(*this, weights, IRT_EQ, orig_weight, opt.icl());

    // Check that all weights from 1 to w (40) can be made.
    // Since the weights can be on either side of the scale,
    // we allow either -1, 0, 1 on the weights, assuming that
    // -1 is the weight on the lef and 1 is on the right.
    for(int w = 1; w <= orig_weight; w++) {
      // note: the range is -weight..weight
      IntVarArray tmp(*this, num_weights, -orig_weight, orig_weight);
      for(int c = 0; c < num_weights; c++) {
        rel(*this, weights[c]*x_m(c,w-1) == tmp[c], opt.icl());
      }
      rel(*this, sum(tmp) == w, opt.icl());
    }

    //
    // Branching.
    //
    branch(*this, weights, INT_VAR_DEGREE_MAX(), INT_VAL_RANGE_MIN());
    branch(*this, x, INT_VAR_DEGREE_MAX(), INT_VAL_MIN());


  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "weights: " << weights << endl;
    Matrix<IntVarArgs> x_m(x, num_weights, orig_weight);
    for(int w = 0; w < orig_weight; w++) {
      cout << w+1 << ": " << x_m.row(w) << endl;
    }
    os << endl;
  }

  // Constructor for cloning s
  BrokenWeights(bool share, BrokenWeights& s) : MinimizeScript(share,s), 
                                                num_weights(s.num_weights), 
                                                orig_weight(s.orig_weight) 
  {
    x.update(*this, share, s.x);
    weights.update(*this, share, s.weights);
  }

  // Return cost
  virtual IntVar cost(void) const {
    return weights[weights.size()-1];
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BrokenWeights(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  BrokenWeightsOptions opt("BrokenWeights");
  opt.solutions(0);

  opt.search(BrokenWeights::SEARCH_BAB);
  opt.search(BrokenWeights::SEARCH_DFS, "dfs");
  opt.search(BrokenWeights::SEARCH_BAB, "bab");
 
  opt.parse(argc,argv);

  if (opt.num_weights() > opt.orig_weight()) {
    cout << "Error: number of weights (" << opt.num_weights() << ") cannot be larger than original weight (" << opt.orig_weight() << endl;
    return 1;
  }

  switch (opt.search()) {
    case BrokenWeights::SEARCH_DFS:
      MinimizeScript::run<BrokenWeights,DFS,BrokenWeightsOptions>(opt); break;
    case BrokenWeights::SEARCH_BAB:
      MinimizeScript::run<BrokenWeights,BAB,BrokenWeightsOptions>(opt); break;
    }

  return 0;

}


