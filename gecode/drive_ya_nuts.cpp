/*

  Drive Ya Nuts puzzle in Gecode.

  From http://www.samstoybox.com/toys/DriveYaNuts.html
  """
  The Drive Ya Nuts puzzle by Milton Bradley was cool and quite difficult. The object of 
  the puzzle is to place all seven nuts such that the numbers on all sides of each 
  nut match the numbers on the adjoining nut. There is but one way to solve the puzzle. 
  Here are two versions of puzzle. Note that the second one is still factory sealed and 
  shows the solution. So you think it sounds easy? 
  """
 
  Some other links: 
  - http://www.jaapsch.net/puzzles/circus.htm
 
  Representation:
 
  A side of a nut is numbered as following
  
             1
 
        6        2
    
        5        3
 
             4
 
 
  and the 7 nuts are numbered as follows:
 
             1 
 
         6       2
             7
         5        3
  
             4
  
  i.e. nut 7 is the master (center) nut.
 
 
  Note: There are 6 solutions, depending on how we orient
        the center nut (7). This is handled by symmetry breaking below.
 
  Here is one solution (which has the center nut start with 1):
  
     2 3 5 1 4 6    Nut 1 (in the representation above)
     3 2 4 1 6 5    Nut 2
     1 4 3 6 5 2    Nut 3
     4 5 6 1 2 3    Nut 4
     2 5 3 1 6 4    Nut 5
     5 4 3 2 1 6    Nut 6
     1 6 2 4 5 3    Nut 7 (center nut)
 
  E.g. the first nut is the nut 1,4,6,2,3,5 rotated like this, i.e.
  with 2 at 12 o'clock and then clock wise: 2,3,5,1,4, and 6:
     
             2
 
        6        3
    
        4        5
 
             1
 
  And so on with the other nuts.
  
  Compare with the MiniZinc model
  http://www.hakank.org/minizinc/drive_ya_nuts.mzn

  Note: I started with this MiniZinc model after reading the Frink 
  implementation by Alan Eliasen 
      http://futureboy.us/fsp/colorize.fsp?f=DriveYaNuts.frink
  which had the link cited above. The Frink program use a different 
  approach, though.
 
  
  [Personal comment: 
   This is the same puzzle as the infamous AWA-Patent problem 
  from a long long time ago, though I didn't then know what 
  it was called.
  Yes, I did solve it manually without any computational help.]
 

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;



class DriveYaNuts : public Script {
protected:

  static const int m = 7;      // number of nuts
  static const int n = 6;      // size of nuts
  static const int num_connections = 12;

  IntVarArray x;   // solutions
  IntVarArray pos; // positions
  IntVarArray pos_inv; // positions
  IntVarArray start_ix; // start indices

public:

  DriveYaNuts(const SizeOptions& opt) 
  : 
    x(*this, m*n, 1, n),
    pos(*this, m, 0, m-1),
    pos_inv(*this, m, 0, m-1),
    start_ix(*this, m, 0, n-1)
  {


    // data

    // in base 1 (fixed below)
    int _connections[] =  {  
      //nuts    sides to be equal
      1,2,       3,6,
      2,3,       4,1,
      3,4,       5,2,
      4,5,       6,3,
      5,6,       1,4,
      6,1,       2,5,
      
      7,1,       1,4,
      7,2,       2,5,
      7,3,       3,6,
      7,4,       4,1,
      7,5,       5,2,
      7,6,       6,3,
    };
    
    IntArgs connections(num_connections*4, _connections);

    // This is the nuts in the solution order.
    // 1,4,6,2,3,5, 1,4,6,2,3,5, // 1 
    // 1,6,5,3,2,4, 1,6,5,3,2,4, // 2 
    // 1,4,3,6,5,2, 1,4,3,6,5,2, // 3
    // 1,2,3,4,5,6, 1,2,3,4,5,6, // 4
    // 1,6,4,2,5,3, 1,6,4,2,5,3, // 5
    // 1,6,5,4,3,2, 1,6,5,4,3,2, // 6 
    // 1,6,2,4,5,3, 1,6,2,4,5,3, // 7 // center nut
    

    // Note that pos_inv for the shown solution is the permutation 
    // [4,3,1,7,5,2,6]
    int _nuts[] = {
      1,2,3,4,5,6, 1,2,3,4,5,6, // 4 (row 4 in the solution order)
      1,4,3,6,5,2, 1,4,3,6,5,2, // 3
      1,4,6,2,3,5, 1,4,6,2,3,5, // 1 
      1,6,2,4,5,3, 1,6,2,4,5,3, // 7 [center nut]
      1,6,4,2,5,3, 1,6,4,2,5,3, // 5
      1,6,5,3,2,4, 1,6,5,3,2,4, // 2 
      1,6,5,4,3,2, 1,6,5,4,3,2, // 6 
    };

    IntArgs nuts1(m*n*2, _nuts);
    Matrix<IntArgs> nuts(nuts1, n*2, m);


    Matrix<IntVarArray> xm(x, n, m);

    for(int i = 0; i < m; i++) {
      distinct(*this, xm.row(i), opt.icl());

      IntVar k(*this, 0, n-1);
      IntVar p(*this, 0, m-1);

      rel(*this, start_ix[i] == k);
      rel(*this, pos[i] == p);

      for(int j = 0; j < n; j++) {
        // x[i,j] == nuts[p, j+k]
   
        // Using matrix element
        IntVar jk(*this, 0, n*2-1);
        rel(*this, jk == j+k);
        element(*this, nuts, jk, p, xm(j,i));

        // Using plain element
        /*
        IntVar pjk(*this, 0, 2*n*m);
        rel(*this, pjk == p*n*2+j+k);
        element(*this, nuts1, pjk, xm(j,i));
        */

      }
    }

    distinct(*this, pos, opt.icl());
    channel(*this, pos, pos_inv);

    // check the connections
    for(int c = 0; c < num_connections; c++) {
      // note: xm(col, row)
      rel(*this, 
          xm(connections[c*4+2]-1,connections[c*4+0]-1) ==
          xm(connections[c*4+3]-1,connections[c*4+1]-1));
    }

    // symmetry breaking:
    // We pick the solution where the center nut (nut 7) start with 1.
    rel(*this, start_ix[m-1] == 0);


    branch(*this, pos, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, start_ix, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "pos (0-based): " << pos << std::endl;
    os << "pos (1-based): ";
    for(int i = 0; i < m; i++) {
      os << pos[i].val()+1 << " ";
    }
    os << std::endl;
    os << "pos_inv (0-based): " << pos_inv << std::endl;
    os << "pos_inv (1-based): ";
    for(int i = 0; i < m; i++) {
      os << pos_inv[i].val()+1 << " ";
    }
    os << std::endl;
    os << "start_ix: " << start_ix << std::endl;
    os << std::endl;
    os << "x: " << std::endl;
    for(int i = 0; i < m; i++) {
      for(int j = 0; j < n; j++) {
        os << x[i*n+j].val() << " ";
      }
      if (i == m-1) {
        os << " [center nut]";
      }
      os << std::endl;
    }


  }

  // Constructor for cloning s
  DriveYaNuts(bool share, DriveYaNuts& s) : Script(share,s) {
    x.update(*this, share, s.x);
    pos.update(*this, share, s.pos);
    pos_inv.update(*this, share, s.pos_inv);
    start_ix.update(*this, share, s.start_ix);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new DriveYaNuts(share,*this);
  }

};


int
main(int argc, char* argv[]) {

    SizeOptions opt("DriveYaNuts");
    opt.icl(ICL_DOM);
    opt.solutions(0);
    opt.parse(argc,argv);
    
    Script::run<DriveYaNuts,DFS,SizeOptions>(opt);

    return 0;
}


