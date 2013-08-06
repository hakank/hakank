/*

  Scheduling (building a house) in Gecode.

  This model is adapted to Gecode from the OPL model sched_intro.mod (examples).
  """
  This is a basic problem that involves building a house. The masonry,
  roofing, painting, etc.  must be scheduled. Some tasks must
  necessarily take place before others, and these requirements are
  expressed through precedence constraints.
  """

  The OPL solution is
  """
  masonry: [0 -- 35 --> 35]
  carpentry: [35 -- 15 --> 50]
  plumbing: [35 -- 40 --> 75]
  ceiling: [35 -- 15 --> 50]
  roofing: [50 -- 5 --> 55]
  painting: [50 -- 10 --> 60]
  windows: [55 -- 5 --> 60]
  facade: [75 -- 10 --> 85]
  garden: [75 -- 5 --> 80]
  moving: [85 -- 5 --> 90]
  """

  See other models:
  * Comet: http://www.hakank.org/comet/building_a_house.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/


/* 

   This Gecode solution:
   """
   masonry: [0 -- 35 --> 35]
   carpentry: [35 -- 15 --> 50]
   plumbing: [35 -- 40 --> 75]
   ceiling: [35 -- 15 --> 50]
   roofing: [50 -- 5 --> 55]
   painting: [50 -- 10 --> 60]
   windows: [55 -- 5 --> 60]
   facade: [75 -- 10 --> 85]
   garden: [75 -- 5 --> 80]
   moving: [85 -- 5 --> 90]
   """

   Note that we use the cumulatives constraints as the "old" cumulative
   constraint. This basically means that all heights are 1 and there
   is just one machine.
  
*/

#include <gecode/driver.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class BuildingAHouse : public MinimizeScript {
protected:

  static const int n = 10;  // number of tasks

  IntVarArray durations;    // durations, so we can print them
  IntVarArray start_times;  // start times
  IntVarArray end_times;    // end times
  IntVarArray machines;     // the machines to be assigned, we just use 1 here.

  IntVar z;                 // number of persons needed, to minimize

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB
  };

  
  BuildingAHouse(const Options& opt) 
    :
    durations(*this, n, 0, 60),
    start_times(*this, n, 0, 120),
    end_times(*this, n, 0, 120),
    machines(*this, n, 0, 0), // only one machine
    z(*this, 0, n*60)
  {


    int _durations[] =  { 35, 15, 40, 15, 5, 10, 5, 10, 5, 5};
    int _height[n]   =  {  1,  1,  1,  1, 1,  1, 1,  1, 1, 1};

    IntArgs height(n, _height);

    // machines
    IntArgs limit(1, 10);

    // copy durations
    for(int i = 0; i < n; i++) {
      rel(*this, durations[i], IRT_EQ, _durations[i], opt.icl());
    }


    enum {masonry,carpentry,plumbing,ceiling,roofing,painting,windows,
          facade,garden,moving};

    //
    // precedences: task i must be finished before task j can begin
    // 
    int num_prec = 14;
    int precedence[] = {
      masonry,   carpentry,
      masonry,   plumbing,
      masonry,   ceiling,
      carpentry, roofing,
      ceiling,   painting,
      roofing,   windows,
      roofing,   facade,
      plumbing,  facade,
      roofing,   garden,
      plumbing,  garden,
      windows,   moving,
      facade,    moving,
      garden,    moving,
      painting,  moving
    };

    for(int i = 0; i < num_prec; i++) {
      rel(*this, end_times[precedence[i*2]], IRT_LQ, start_times[precedence[i*2+1]], opt.icl());
    }

    bool atmost = true;
    cumulatives(*this, machines, start_times, durations, end_times, height,
                limit, atmost, opt.icl());

    // calculate z (to minimize)
    // here we take the last task: the end of moving
    rel(*this, end_times[moving], IRT_EQ, z, opt.icl());

    if (opt.search() == BuildingAHouse::SEARCH_DFS) {
      rel(*this, z <= 90, opt.icl());
    }

    branch(*this, machines, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, end_times, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, start_times, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "z: " << z << std::endl;
    /*
    os << "machines   : " << machines << std::endl;
    os << "start_times: " << start_times << std::endl;
    os << "end_times  : " << end_times << std::endl;
    */
    std::string task_str[] = {"masonry","carpentry","plumbing","ceiling",
                               "roofing","painting","windows","facade",
                               "garden","moving"};
  
    for(int i = 0; i < n; i++) {
      os << task_str[i] << ": [" << start_times[i] << " -- " << durations[i] << " --> " << end_times[i] << "]" << std::endl;
    }

  }

  // Return cost
  virtual IntVar cost(void) const {
    return z;
  }


  // Constructor for cloning s
  BuildingAHouse(bool share, BuildingAHouse& s) : MinimizeScript(share,s) {
    durations.update(*this, share, s.durations);
    start_times.update(*this, share, s.start_times);
    end_times.update(*this, share, s.end_times);
    machines.update(*this, share, s.machines);
    z.update(*this, share, s.z);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BuildingAHouse(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("BuildingAHouse");
  opt.solutions(0);
  opt.iterations(200);
  opt.search(BuildingAHouse::SEARCH_BAB);
  opt.search(BuildingAHouse::SEARCH_DFS, "dfs");
  opt.search(BuildingAHouse::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  if (opt.search() == BuildingAHouse::SEARCH_DFS) {
    Script::run<BuildingAHouse, DFS, Options>(opt);
  } else {
    MinimizeScript::run<BuildingAHouse, BAB, Options>(opt);
  }


  return 0;
}


