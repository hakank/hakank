/*

  Simple bin packing in Gecode.

  Some simple bin packing problems.

  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/bin_packing.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/bin_packing.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/bin_packing.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <algorithm>

using namespace Gecode;

using std::cout;
using std::endl;

namespace {

  // List of problems
  extern const int* problems[];
  // Number of specifications
  extern const unsigned int n_examples;

}


class BinPacking : public MinimizeScript {
protected:

  // Problem to be used
  const int* prob;

  // Return rows of matrix
  int get_num_stuff(void) const {
    return prob[0];
  }

  int get_bin_capacity(void) const {
    return prob[1];
  }
  
  IntVarArray bins;   
  IntVarArray bin_loads;   

  IntVar num_loaded_bins;

public:

  // Search variants
  enum {
    SEARCH_BAB,     // Branch & bound
    SEARCH_RESTART, // Restart
    SEARCH_DFS,     // Use depth first search
  };

  // Symmetry options
  enum {
    SYMMETRY_NONE, // no symmetry breaking 
    SYMMETRY_SORT  // sort the stuff array first
  };

  BinPacking(const SizeOptions& opt) 
  : 
    prob(problems[opt.size()]),
    bins(*this, get_num_stuff()*get_num_stuff(), 0, 1),
    bin_loads(*this, get_num_stuff(), 0, get_bin_capacity()),
    num_loaded_bins(*this, 0, get_num_stuff())
  {

    int num_stuff = get_num_stuff();
    int num_bins = num_stuff;

    int bin_capacity = get_bin_capacity();

    int p = 2;
    int _stuff_orig[num_stuff];
    int _stuff[num_stuff];
    for(int i = 0; i < num_stuff; i++) {
      _stuff_orig[i] = prob[p++];
    }

    // symmetry breaking: sort the stuff first
    if (opt.symmetry() == SYMMETRY_SORT) {
      std::sort(_stuff_orig, _stuff_orig+num_stuff);
    }

    // and copy 
    for(int i = 0; i < num_stuff; i++) {
      _stuff[i] = _stuff_orig[i];
    }
    IntArgs stuff(num_stuff, _stuff);
    cout << "stuff: " << stuff << endl;

    int sum_stuff = 0;
    for(int s = 0; s < num_stuff; s++) {
      sum_stuff += _stuff[s];
    }

    // sanity clause: No thing can be larger than capacity.
    for(int s = 0; s < num_stuff; s++) {
      rel(*this, stuff[s] <= IntVar(*this, bin_capacity, bin_capacity));
    }

    // the total load in the bin cannot exceed bin_capacity
    for(int b = 0; b < num_bins; b++) {
      rel(*this, bin_loads[b] <= bin_capacity);
      IntVarArgs tmp;
      for(int s = 0; s < num_stuff; s++) {
        tmp << expr(*this, stuff[s]*bins[b*num_stuff+s]);
      }
      rel(*this, bin_loads[b] == sum(tmp));
    }

    // calculate the total load for a bin
    rel(*this, sum_stuff == sum(bin_loads));

    // a thing is packed just once 
    for(int s = 0; s < num_stuff; s++) {
      IntVarArgs tmp;
      for(int b = 0; b < num_bins; b++) {
        tmp << expr(*this, bins[b*num_stuff+s]);
      }
      rel(*this, sum(tmp) == 1);
    }

    // symmetry breaking: 
    // if bin_loads[i+1] is > 0 then bin_loads[i] must be > 0
    for(int b = 0; b < num_bins - 1; b++) {
      rel(*this, (bin_loads[b+1] > 0) >> (bin_loads[b] > 0));
      rel(*this, bin_loads[b] >= bin_loads[b+1]);
    }

    // symmetry breaking: first bin must be loaded
    rel(*this, bin_loads[0] > 0);

    // calculate num_loaded_bins
    BoolVarArgs num_loaded_bins_tmp;
    for(int b = 0; b < num_bins; b++) {
      num_loaded_bins_tmp << expr(*this, bin_loads[b] > 0);
    }
    rel(*this, num_loaded_bins == sum(num_loaded_bins_tmp));


    branch(*this, bin_loads, INT_VAR_SIZE_MAX(), INT_VAL_MIN());
    branch(*this, bins, INT_VAR_SIZE_MAX(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "num_loaded_bins: " << num_loaded_bins << std::endl;
    os << "bin_loads: " << bin_loads << std::endl;
    int num_stuff = get_num_stuff();
    for(int b = 0; b < num_loaded_bins.val(); b++) {
      for(int s = 0; s < num_stuff; s++) {
        cout << bins[b*num_stuff+s] << " ";
      }
      os << endl;
    }
    os << endl;
  }

  // Constructor for cloning s
  BinPacking(bool share, BinPacking& s) : MinimizeScript(share,s), prob(s.prob) {
    bins.update(*this, share, s.bins);
    bin_loads.update(*this, share, s.bin_loads);
    num_loaded_bins.update(*this, share, s.num_loaded_bins);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BinPacking(share,*this);
  }

  virtual IntVar cost(void) const {
    return num_loaded_bins;
  }

};


int
main(int argc, char* argv[]) {
  SizeOptions opt("BinPacking");
  opt.solutions(0);

  opt.search(BinPacking::SEARCH_RESTART, "restart");
  opt.search(BinPacking::SEARCH_BAB, "bab");
  opt.search(BinPacking::SEARCH_DFS, "dfs");

  opt.symmetry(BinPacking::SYMMETRY_NONE);
  opt.symmetry(BinPacking::SYMMETRY_NONE, "none", "do not use symmetry");
  opt.symmetry(BinPacking::SYMMETRY_SORT, "sort", "sort the stuff array");

  opt.size(0); // choose first example

  opt.parse(argc,argv);

  if (opt.size() > n_examples-1) {
    std::cerr << "example must be between 0 and " << n_examples-1 << std::endl;
    return 1;
  }

  switch (opt.search()) {
    case BinPacking::SEARCH_DFS:
      MinimizeScript::run<BinPacking,DFS,SizeOptions>(opt); break;
    case BinPacking::SEARCH_BAB:
      MinimizeScript::run<BinPacking,BAB,SizeOptions>(opt); break;

    }

  return 0;

}


namespace {

  /** Problem specifications
   *  Each problem instance defines the following 
   *
   *    num_stuff,
   *    bin_capacity,
   *    <the stuff to pack>
   *
   */

  /*
    Example from the Alice system
    Copying files to disks
    http://news.mozart-oz.org/alice/manual/cptutorial/node55.html
    """
    Suppose, you want to copy a set of files from your hard-disk onto as 
    few as possible diskettes of a given size, e. g. onto common 1.44 MB 
    diskettes. In case your files do not fit on a single diskette, it might 
    become quite tricky to figure out the minimal number of needed diskettes 
    and how to partition the files.
    """
  */
  const int p0[] =
    {
      6,
      1404,
      360, 850, 630, 70, 700, 210
    };


  // simple (and probably unrealistic) packing
  const int p1[] = 
    {
      20,
      20,
      1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
    };


  // simple (and probably even less unrealistic) packing
  const int p2[] = 
    { 
      25,
      50,
      1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25
    };

  /*
    This problem below is from
    http://www.dcs.gla.ac.uk/~pat/cpM/slides/binPacking.ppt
    1D Bin Packing (or "CP? Who cares?"), page 3
    and also from
    http://www.dcs.gla.ac.uk/~pat/cpM/JChoco/binPack
  */
  const int p3[] = 
    {
      10,
      150,
      42,69,67,57,93,90,38,36,45,42
    };

  // same source of data, but now there are 22 things
  const int p4[] = 
    {
      22,
      250,
      42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
         46,38,85,33
    };

  // continuing of the above example.
  const int p5[] = 
    {
      50,
      290,
      42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
      46,38,85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,
      30,64,29,74,41,49,55,98,80,32,25,38,82,30
    };

  // ibid. 
  const int p6[] =
    {
      100,
      500,
      42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,
      85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,30,64,29,74,
      41,49,55,98,80,32,25,38,82,30,35,39,57,84,62,50,55,27,30,36,
      20,78,47,26,45,41,58,98,91,96,73,84,37,93,91,43,73,85,81,79,
      71,80,76,83,41,78,70,23,42,87,43,84,60,55,49,78,73,62,36,44,
      94,69,32,96,70,84,58,78,25,80,58,66,83,24,98,60,42,43,43,39
    };


  // From 
  // Graham Kendall: Bin Packing made Easier 
  // http://research-reflections.blogspot.com/2009/07/bin-packing-made-easier.html
  const int p7[] = 
    {
      33,
      524,
      442,252,127,106,37,10,10,252,252,127,106,37,10,9,
      252,252,127,85,12,10,9,252,127,106,84,12,10,252,
      127,106,46,12,10
    };

  // Variant: remove 46 from the problem above
  const int p8[] =
    {
      32,
      524,
      442,252,127,106,37,10,10,252,252,127,106,37,10,9,
      252,252,127,85,12,10,9,252,127,106,84,12,10,252,
      127,106,   12,10
    };
  
  const int *problems[] = {p0, p1, p2, p3, p4, p5, p6, p7, p8};
  const unsigned n_examples = sizeof(problems)/sizeof(int*);

}
