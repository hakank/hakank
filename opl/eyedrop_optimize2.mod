/* 

  Optimize eye drops in OPL.

  After a Cataract operation that didn't go well - they 
  didn't put back a lens - I am ordinated three different 
  eye drops to take each day (for a couple of some weeks from now):

    6 drops per day of kind 1 (later: 4 drops)
    3 drops per day of kind 2
    1 drop per day of kind 3 (for the night)

  What is the (an) optimal way of distributing these drops, provided:

   - the day start at 7:00 and end at 23:00
   - the different types should be distributed as "even as possible"
     (whatever that mean)
   - there may just be one drop per hour
   - drop 3 is taken last ("for the night")

  The objective is then to separate the times to take an eye drop type 
  as much as possible (variables z is the sum of dispersion).


  Here is one solution to the original ([6,3,1]) problem:

    z:   : 36
    x    : [1, 2, 0, 1, 0, 0, 1, 2, 0, 1, 0, 0, 1, 2, 0, 1, 3]
    dists: [30, 6, 0]
     7: 1
     8: 2
     9:   
    10: 1
    11:   
    12:   
    13: 1
    14: 2
    15:   
    16: 1
    17:   
    18:   
    19: 1
    20: 2
    21:   
    22: 1
    23: 3
  
    Type 1: 7 10 13 16 19 22 
    Type 2: 8 14 20 
    Type 3: 23 
  
  Another solution also with z = 36 (optimal):

    Type 1: 7 10 13 16 19 22 
    Type 2: 9 15 21 
    Type 3: 23 



  Here is one solution to the [4,3,1] problem:
  
    z:   : 21
    x    : [1, 2, 0, 0, 0, 1, 0, 2, 0, 0, 1, 0, 0, 2, 0, 1, 3]
    dists: [15, 6, 0]
    ...
    Type 1: 7 12 17 22 
    Type 2: 8 14 20 
    Type 3: 23 

  


  For more about Cataract surgery:
  https://en.wikipedia.org/wiki/Cataract_surgery



  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int start = ...; // start hour
int endx = ...;  // end hour
int num_types = ...; // number of different types of eye drops
int occ[1..num_types] = ...; // occurrences of each type


// Original ordination
/*
int start = 7; // start hour
int endx = 23;  // end hour
int num_types = 3; // number of different types of eye drops
int occ[1..num_types] = [6,3,1]; // occurrences of each type
*/

// Later ordination
/*
int start = 7; // start hour
int endx = 23;  // end hour
int num_types = 3; // number of different types of eye drops
int occ[1..num_types] = [4,3,1]; // occurrences of each type
*/

// Test1
/*
int start = 7; // start hour
int endx = 23;  // end hour
int num_types = 5; // number of different types of eye drops
int occ[1..num_types] = [6,5,3,2,1]; // occurrences of each type
*/

// Test 2
/*
int start = 1; // start hour
int endx = 30;  // end hour
int num_types = 6; // number of different types of eye drops
int occ[1..num_types] = [8,5,4,3,2,1]; // occurrences of each type
*/

int hours = endx - start + 1;
int max_times = max(t in 1..num_types) occ[t];


// decision variables
// The time table. 0 means that there is no drop to be taken this hour.
dvar int x[start..endx] in 0..num_types;
// total dispersion 
dvar int z in 0..1000;

// The time each drop type should be taken
dvar int drop_table[1..num_types, 1..max_times] in start..endx;
// The time differences for each drop type
dvar int dists_table[1..num_types, 1..max_times-1] in 0..hours*num_types;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Basic"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(drop_table,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  var s2 = f.searchPhase(dists_table,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s, s2);


}

maximize z;
constraints {

   // The occurrences of each drop
   forall(t in 1..num_types) {
        count(x, t) == occ[t];
   }

   forall(t in 1..num_types) {
      allDifferent(all(i in 1..occ[t]) drop_table[t,i]);

      forall(i in 2..occ[t]) drop_table[t,i-1] <= drop_table[t, i];

      forall(i in 1..occ[t]-1) dists_table[t, i] == drop_table[t,i+1]-drop_table[t,i];

      // Connect the time table (x) with the times of this drop type.
      forall(i in start..endx) { 
          (x[i] == t) => sum(j in 1..occ[t]) (drop_table[t,j] == i) >= 1; 
      }


      // fix the unknowns (after occ[t]) in drop_table and dists_table since
      // they are not used for this drop
      forall(j in occ[t]+1..max_times) {
        drop_table[t,j] == start;
      }

      forall(j in occ[t]..max_times-1) {
        dists_table[t,j] == 0;
      }

   }


   // Heuristic to maximize the dispersion
   z == sum(t in 1..num_types, i,j in 1..occ[t]-1: i < j) (dists_table[t,j] * (dists_table[t,j] == dists_table[t,i]));

   // Specific constraint: 
   // Type 3 is the last drop to take (not necessarily at the last hour)
   // Note: It assume a single occurrence of the drop type.
   if (occ[num_types] == 1) {
     forall(i in start..endx) {
         (x[i] == num_types) => sum(j in i+1..endx) (x[j] > 0) == 0;
     }
   }

   // Checking
   // z == 36; // Original ordination

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var t = thisOplModel;
      writeln("z: ", t.z);
      writeln("x: ", t.x);

      for(i = t.start; i <= t.endx; i++) {
        if (i < 10) { write(" "); }
        write(i, ": ");
        if (t.x[i] > 0) {
          write(t.x[i]);
        }
        writeln();
      }      

      for(i = 1; i <= t.num_types; i++) {
         write("Drop type: ", i, ": ")
         for(j = 1; j <= t.occ[i]; j++) {
            write(t.drop_table[i][j], " ");
         }      
         writeln();  
      }

      for(i = 1; i <= t.num_types; i++) {
         write("Dist type: ", i, ": ")
         for(j = 1; j <= t.occ[i]-1; j++) {
            write(t.dists_table[i][j], " ");
         }      
         writeln();  
      }

      writeln();
      sol++;
   }

   cp.endSearch();

   writeln();
   var info = cp.info;
   writeln("#ModelVariables: ", info.NumberOfModelVariables);
   writeln("#Variables: ", info.NumberOfVariables);
   writeln("#Constraints: ", info.NumberOfConstraints);
   writeln("#Choice points: ", info.NumberOfChoicePoints);
   writeln("#Fails: ", info.NumberOfFails);
   writeln("#Branches: ", info.NumberOfBranches);
   writeln("TotalTime: ", info.TotalTime);
   writeln("SolveTime: ", info.SolveTime);
   writeln();

   writeln("#solutions: ", sol);

} 
