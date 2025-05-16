/* 

  Combinatorial auction problem in OPL.

  http://en.wikipedia.org/wiki/Combinatorial_auction
  """
  A combinatorial auction is an auction in which bidders can place 
  bids on combinations of items, or "packages," rather than 
  just individual items. Simple combinatorial auctions have been 
  used for many years in estate auctions, where a common procedure 
  is to auction the individual items and then at the end to accept 
  bids for packages of items.
  """

  This simple example is from the lecture slides
  Constraint Satisfaction Problems, Constraint Optimization
  by Bernhard Nebel and Stefan Wölfl
  http://www.informatik.uni-freiburg.de/~ki/teaching/ws0910/csp/csp10-handout4.pdf
  """
  In combinatorial auctions, bidders can give bids for set of items.
  The auctioneer [then] has to generate an optimial selection, e.g.
  one that maximizes revenue.
  
  Definition
  The combinatorial auction problem  is specified as follows:
    Given: A set of items Q = {q1,...,qn} and a set of bids
           B = {b1,...,bm} such that each bid is bi = (Qi, ri),
           where Qi (= Q and ri is a strictly positive real number.
    Task: Find a subset of bids B'(= B such that any two bids in B'
          do not share an item maximizing Sum(Qi,ri) (= Biri.

  ...

  Example Auction

  Consider the following auction:
    b1 = {1,2,3,4}, r1 = 8
    b2 = {2,3,6},   r2 = 6
    b3 = {1,4,5},   r3 = 5
    b4 = {2,8},     r4 = 2
    b5 = {5,6},     r5 = 2

  What is the optimal assignment?
  """ 

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int num_items = ...;
int num_bids = ...;
int max_item = ...;
{int} packages[1..num_bids] = ...;
int bids[1..num_bids] = ...;


/*
int num_items = 7;
int num_bids = 5;
int max_item = 7;
{int} packages[1..num_bids] = 
[
   {1,2,3,4},
   {2,3,6},
   {1,4,5},
   {2,7},
   {5,6}
];

int bids[1..num_bids] = [8,6,5,2,2];
*/


// decision variables
dvar int x[1..num_bids] in 0..1;
dvar int total;


execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);
 
}

maximize total;
constraints {

   total == sum(i in 1..num_bids) x[i]*bids[i];

   // ensure that each items is selected atmost once
   forall(j in 1..num_items) {
       sum(i in 1..num_bids) x[i]*(j in packages[i]) <= 1;
   }


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("total:", t.total);
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
