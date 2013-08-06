/*  

  Lichtenstein coloring problem in Gecode.

  From 
  http://bit-player.org/2008/the-chromatic-number-of-liechtenstein
  and
  """
  It seems that Liechtenstein is divided into 11 communes, which 
  emphatically do not satisfy the connectivity requirement of the four 
  color map theorem. Just four of the communes consist of a single 
  connected area (Ruggell, Schellenberg and Mauren in the north, and 
  Triesen in the south). 
  ...
  In the map above, each commune is assigned its own color, and so we 
  have an 11-coloring. It’s easy to see we could make do with fewer 
  colors, but how many fewer? I have found a five-clique within the map; 
  that is, there are five communes that all share a segment of border 
  with one another. It follows that a four-coloring is impossible. Is 
  there a five-coloring? What is the chromatic number of Liechtenstein?
  """

  Also see
  http://blog.mikael.johanssons.org/archive/2008/10/on-the-chromatic-number-of-lichtenstein/

  Thanks to Mikael Zayenz Lagerkvist for some improvements of the model:
   - using BoolVarArgs for color_used
   - two alternative version for stating that color_used should be ordered
     (1's before 0's).
  
  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class Coloring : public MinimizeScript {
protected:


  static const int num_communes = 11;
  static const int num_colors = 11; // 6;
  static const int num_enclaves = 29;

  // color for the en-/exclaves
  IntVarArray color;  
  // colors for the communes
  IntVarArray color_communes;

  // IntVarArray color_used;
  BoolVarArray color_used;

  // number of color used (to be minimized)
  IntVar n_colors;


public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search to find the smallest tick
    SEARCH_BAB     // Use branch and bound to optimize
  };

  Coloring(const SizeOptions& opt) 
  :    
    color(*this, num_enclaves, 0, num_colors-1),
    color_communes(*this, num_colors, 0, num_colors-1),
    color_used(*this, num_colors, 0, 1),
    n_colors(*this, 0, num_colors-1)

  {

    enum communes {
      BalzersC,
      EschenC,
      GamprinC,
      MaurenC,
      PlankenC,
      RuggellC,
      SchaanC,
      SchellenbergC,
      TriesenC,
      TriesenbergC,
      VaduzC
    };

    enum enclaves_exclaves {
      Balzers1, Balzers2, Balzers3,
      Eschen1,  Eschen2,  Eschen3,
      Gamprin1, Gamprin2,
      Mauren,
      Planken1, Planken2, Planken3, Planken4,
      Ruggell,
      Schaan1, Schaan2, Schaan3, Schaan4, Schaan5,
      Schellenberg,
      Triesen,
      Triesenberg1, Triesenberg2,
      Vaduz1, Vaduz2, Vaduz3, Vaduz4,Vaduz5, Vaduz6
    };

    // 
    // the enclaves and corresponding commune
    //
    int _cc[] = 
      {
        BalzersC, BalzersC, BalzersC, 
        EschenC, EschenC, EschenC, 
        GamprinC, GamprinC, 
        MaurenC, 
        PlankenC, PlankenC, PlankenC, PlankenC, 
        RuggellC, 
        SchaanC, SchaanC, SchaanC, SchaanC, SchaanC, 
        SchellenbergC, 
        TriesenC, 
        TriesenbergC, TriesenbergC, 
        VaduzC, VaduzC, VaduzC, VaduzC, VaduzC, VaduzC
      };
    IntArgs cc(num_enclaves, _cc);

    // This map of neighbours is from
    // http://blog.mikael.johanssons.org/archive/2008/10/on-the-chromatic-number-of-lichtenstein/
    // 
    int num_edges = 45;
    int _Lichtenstein[] = 
      {
        Ruggell, Schellenberg,
        Ruggell, Gamprin1,
        Schellenberg, Mauren,
        Schellenberg, Eschen1,
        Mauren, Eschen1,
        Gamprin1, Eschen2,
        Gamprin1, Vaduz2,
        Gamprin1, Schaan1,
        Gamprin1, Planken3,
        Gamprin1, Eschen1,

        Eschen1, Gamprin2,
        Eschen1, Planken1,
        Eschen2, Schaan1,
        Vaduz3, Schaan1,
        Vaduz2, Schaan1,
        Planken3, Schaan1,
        Planken2, Schaan1,
        Schaan1, Planken1,
        Schaan1, Planken4,
        Schaan1, Vaduz1,

        Gamprin2, Eschen3,
        Eschen3, Vaduz4,
        Eschen3, Schaan2,
        Vaduz4, Schaan2,
        Vaduz4, Planken1,
        Schaan2, Planken1,
        Planken1, Schaan3,
        Vaduz1, Triesenberg1,
        Vaduz1, Triesen,
        Planken4, Triesenberg1,

        Planken4, Balzers2,
        Balzers2, Vaduz5,
        Balzers2, Schaan4,
        Vaduz5, Schaan4,
        Schaan4, Triesenberg1,
        Schaan4, Vaduz6,
        Schaan4, Triesenberg2,
        Triesenberg1, Vaduz6,
        Triesenberg1, Triesen,
        Triesenberg1, Balzers3,

        Triesen, Balzers3,
        Triesen, Balzers1,
        Triesen, Schaan5,
        Vaduz6, Schaan5,
        Triesenberg2, Schaan5
      };
    IntArgs Lichtenstein(num_edges*2, _Lichtenstein);


    /*
      Symmetry breaking I
      First commune (Balzers) has color 0
    */
    rel(*this, color_communes[BalzersC] == 0);
    /*
      Symmetry breaking II: take colors in order
    */
    /*
    for(int c = 1; c < num_colors; c++) {
      rel(*this, (color_used[c]) >> (color_used[c-1]));
    }
    */
    // alternative versions suggested by Mikael Zayenz Lagerkvist
    rel(*this, color_used, IRT_GQ);
    // extensional(*this, color_used, *REG(1) + *REG(0));

    /*
      Is a color used?
    */
    for(int i = 0; i < num_colors; i++) {
      BoolVarArgs tmp(*this, num_enclaves, 0,1);
      for(int c = 0; c < num_enclaves; c++) {
        tmp[c] = expr(*this, color[c]==i);
      }
      // we must test both cases and set color_used 
      // to either true of false
      rel(*this, (sum(tmp) > 0) >> (color_used[i]));
      rel(*this, (sum(tmp) == 0) >> (!color_used[i]));
    }

    /*
      All neighbours must have different colors
    */
    for(int i = 0; i < num_edges; i++) {
      int l1 = Lichtenstein[i*2+0];
      int l2 = Lichtenstein[i*2+1];
      rel(*this, color[l1] != color[l2]);
    }

    /*
       Exclaves of the same commune must have the same color
    */
    for(int i = 0; i < num_enclaves; i++) {
      for(int j = 0; j < i; j++) {
        if (cc[i] == cc[j]) {
          rel(*this, color[i] == color[j]);
        }
      }
    }

    /*
      Connection between commune and en-/exclaves
    */
    for(int c = 0; c < num_communes; c++) {
      BoolVarArgs tmp(*this, num_enclaves, 0, 1);
      for(int e = 0; e < num_enclaves; e++) {
        rel(*this, tmp[e] == (color_communes[c] == color[e] && IntVar(*this, cc[e], cc[e]) == IntVar(*this, c,c)));
      }
      rel(*this, sum(tmp) > 0);
    }

    // Show all (minimal) solutions if DFS
    if (opt.search() == SEARCH_DFS) {
      rel(*this, n_colors == 5); 
    }

    // number of colors used
    rel(*this, n_colors == sum(color_used));

    // Branching
    branch(*this, color, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, color_communes, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    // branch(*this, color_used, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

    branch(*this, n_colors, INT_VAL_MAX());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "n_colors: " << n_colors << endl;
    os << "color: " << color << endl;
    os << "color_communes: " << color_communes << endl;
    os << "color_used: " << color_used << endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Coloring(bool share, Coloring& s) : MinimizeScript(share, s) {
    color.update(*this, share, s.color);
    color_communes.update(*this, share, s.color_communes);
    color_used.update(*this, share, s.color_used);
    n_colors.update(*this, share, s.n_colors);

  }

  // Return cost
  virtual IntVar cost(void) const {
    return n_colors;
  }


  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Coloring(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Coloring");
  opt.solutions(0);

  opt.search(Coloring::SEARCH_BAB, "bab");
  opt.search(Coloring::SEARCH_DFS, "dfs");
 
  opt.parse(argc,argv);

  switch (opt.search()) {
    case Coloring::SEARCH_DFS:
      MinimizeScript::run<Coloring,DFS,SizeOptions>(opt); break;
    case Coloring::SEARCH_BAB:
      MinimizeScript::run<Coloring,BAB,SizeOptions>(opt); break;
  }

  return 0;

}


