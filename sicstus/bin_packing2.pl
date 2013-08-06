/*

  Global constraint bin_packing in SICStus Prolog.

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

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/bin_packing2.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/bin_packing2.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

/*
  From the ECLiPSe model:
  """
  This is also a study of translating a simple predicate from MiniZinc to 
  ECLiPSe. Here is the MiniZinc code, n is the size of weights and bins:
   forall(b in 1..n) (
      sum(j in 1..n) ( weights[j]*bool2int(bins[j] = b)) <= capacity
   )
 
  Using the same approach as MiniZinc, I prefer version 2 to version 1,
  since it is more succint. I prefer version 2 to version 3 since it
  is clearer.
  

  Note that both the MiniZinc version and this ECLiPSe version are
  fully multidirectional, i.e. we can let either (or all) of the
  variables be free and it still work.
  If we let Bins free, we must fix the length of it (to 3 in the
  example).
  """
*/


% version 1, straightforward
bin_packing(Bins, Weights, Capacity) :-
        length(Bins,N),
        length(Weights,N),

        ( for(B1,1,N),
          param(Weights,Bins,N,Capacity) do
              ( for(J,1,N),
                fromto(0,In,Out,Sum),
                param(Weights,Bins,B1) do
                    element(J,Bins,BinsJ),
                    BinsJ #= B1
              -> 
                element(J,Weights,WeightsJ),
                Out #= In + WeightsJ
              ; 
                Out = In
              ),
              Sum #=< Capacity
        ).

% version 2, using reification like bool2int.
bin_packing2(Bins, Weights, Capacity) :-
        length(Bins,N),
        length(Weights,N),

        ( for(B2,1,N),
          param(Weights,Bins,N,Capacity) do
             ( for(J,1,N),
               fromto(0,In,Out,Sum),
               param(Weights,Bins,B2) do
                   element(J,Weights,WeightsJ),
                   element(J,Bins,BinsJ),
                   Z in 0..1,
                   BinsJ #= B2 #<=> Z #= 1,
                   Out #= In + WeightsJ * Z
             ),
             Sum #=< Capacity
        ).


% bin_packing3 is not relevant for SICStus

%
% version 4: Use eval and a foreach(S, Sum)      
bin_packing4(Bins, Weights, Capacity) :-
        length(Bins,N),
        length(Weights,N),

        ( for(B4,1,N),
          param(Weights,Bins,N,Capacity) do
              (for(J,1,N),
               foreach(S,Sum),
               param(Weights,Bins,B4) do
                   element(J,Weights,WeightsJ),
                   element(J,Bins,BinsJ),
                   Z in 0..1,
                   BinsJ #= B4 #<=> Z #= 1,
                   S #= WeightsJ*Z
              ),
              sum(Sum,#=<,Capacity)
        ).


%
% This version is without the for/3 loop constructs 
% and the need of element/3.
%
% Instead it loops through all distinct values in Bins 
% and sums the weights of each occurrence.
%
bin_packing5(Bins,Weights,Capacity) :-
        sort(Bins,BinsUnique),
        ( foreach(B5,BinsUnique),
          param(Weights,Capacity,Bins) do
              ( foreach(Weight,Weights),
                foreach(BTmp,Bins),
                fromto(0,In,Out,Sum),
                param(B5) do
                    Z in 0..1,
                    BTmp #= B5 #<=> Z #= 1,
                    Out #= In + Weight*Z
              ),
              Sum #=< Capacity
        ).


go :-
        % we let something be unknown
        problem(2,Bins, Weights,_Capacity),
        % N = 3, % if Bins is free we must set the length of it
        length(Bins,N),
        domain(Bins,1,N),

        length(Weights, N),
        domain(Weights,1,4),

        % Capacity = 5,
        Capacity in 1..20,

        % bin_packing(Bins,Weights,Capacity),
        % bin_packing2(Bins,Weights,Capacity),
        % bin_packing4(Bins,Weights,Capacity),
        bin_packing5(Bins,Weights,Capacity),
        
        % append(Weights,Bins,Vars),
        % append([Capacity],Vars,Vars1),
        labeling([],[Capacity]),

        write(capacity:Capacity),nl,
        write(bins:Bins),nl,
        write(weights:Weights),nl,
        fd_statistics(backtracks,Backtracks),
        write(backtracks:Backtracks),nl,nl.
        %, fail.
 
       
%
% problem(problem_id, Bins, Weights, Capacity)
%

% example above from Global Constraint Catalog
problem(1, 
        [3,1,3], % bins
        [4,3,1], % weights
        5        % capacity
       ).

% another example
problem(2, [3,1,3,2,2,1,2,3], [4,3,1,3,4,3,1,2], 5).
