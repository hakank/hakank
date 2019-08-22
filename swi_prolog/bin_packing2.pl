/*

  Global constraint bin_packing in SWI Prolog

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/sec4.35.html
  """
  Given several items of the collection ITEMS (each of them having a specific 
  weight), and different bins of a fixed capacity, assign each item to a bin 
  so that the total weight of the items in each bin does not exceed CAPACITY.
  
  Example
    <(5,<bin-3 weight-4, bin-1 weight-3,bin-3 weight-1>)>
  
   The bin_packing constraint holds since the sum of the height of items 
  that are assigned to bins 1 and 3 is respectively equal to 3 and 5. 
  The previous quantities are both less than or equal to the maximum 
  CAPACITY 5. Figure 4.35.1 shows the solution associated with the example.
  
  Remark
  
  Note the difference with the classical bin-packing problem [MT90] where 
  one wants to find solutions that minimise the number of bins. In our 
  case each item may be assigned only to specific bins (i.e., the different 
  values of the bin variable) and the goal is to find a feasible solution. 
  This constraint can be seen as a special case of the cumulative 
  constraint [AB93], where all task durations are equal to 1.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        member(Problem,[1,2]),
        writeln(problem=Problem),
        once(do_problem(Problem)),
        fail,
        nl.

go.

 
do_problem(Problem) :-
       
        %% we let the Capacity be unknown since it should be minimized
        problem(Problem,Bins, Weights,_CapacityOrig),       

        length(Weights,N),
        length(Bins,N),
        
        Capacity in 1..20,
        
        bin_packing(Bins,Weights,Capacity),
        
        flatten([Weights,Bins,Capacity],Vars),
        labeling([min(Capacity)],Vars),

        writeln(capacity=Capacity),
        writeln(bins=Bins),
        writeln(weights=Weights),
        nl.
 
 
bin_packing(Bins, Weights, Capacity) :-
        length(Bins,N),
        length(Weights,N),
        numlist(1,N,Bs),
        maplist(bin_packing_(Weights,Bins,Capacity),Bs),
        nl.

%%foreach(B in 1..N)
%%   sum([(Weights[J]*(Bins[J] #= B)) : J in 1..N ]) #=< Capacity
%%end,
bin_packing_(Weights,Bins,Capacity,B) :-
        sum_b(Bins,Weights,Capacity,B,0,Sum),
        Sum #=< Capacity.

sum_b([],_Weights,_Capacity,_B,Sum,Sum).
sum_b([Bin|Bins],[Weight|Weights],Capacity,B,Sum0,Sum) :-
        BB in 0..1,
        Bin #= B #<==> BB #= 1,
        Sum1 #= Sum0 + Weight*BB,
        sum_b(Bins,Weights,Capacity,B,Sum1,Sum).

%% The example cited above
problem(1, Bins, Weights, Capacity) :-
        Bins = [3,1,3],
        Weights = [4,3,1],
        Capacity = 5.

%% another example
problem(2, Bins, Weights, Capacity) :- 
        Bins = [3,1,3,2,2,1,2,3],
        Weights = [4,3,1,3,4,3,1,2],
        Capacity = 5.
