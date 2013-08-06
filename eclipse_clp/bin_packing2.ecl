/*

  Global constraint bin_packing in ECLiPSe.

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

  Compare to the MiniZinc model http://www.hakank.org/minizinc/bin_packing2.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
% :-lib(propia).


/*
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
*/


%% version 1, straightforward.
%% Note: This version is not correct! 
%% See Joachim Schimpf's comment from the ECLiPSe mailing list
%% http://sourceforge.net/mailarchive/forum.php?thread_name=4C50E9FC.9020709%40monash.edu&forum_name=eclipse-clp-users
%% """
%% The intention of this code is to set up the capacity constraint for
%% a bin, by adding up the weigths of all objects J that are being
%% put into bin B - that's what Bins[J] #= B means.
%% 
%% This code is actually wrong, because you cannot use a constraint
%% inside the condition of an if-then-else.
%% """
%% However, it gives the correct answer... 
% bin_packing1(Bins, Weights, Capacity) :-
%         dim(Bins,[N]),
%         dim(Weights,[N]),
%
%         (for(B,1,N),
%          param(Weights,Bins,N,Capacity) do
%              (for(J,1,N),
%              fromto(0,In,Out,Sum),
%              param(Weights,Bins,B) do
%                   Bins[J] #= B 
%              -> 
%               Out #= In + Weights[J] 
%              ; 
%               Out = In
%              ),
%              Sum #=< Capacity
%         ).

%
% version 2, using reification like bool2int.
%
bin_packing2(Bins, Weights, Capacity) :-
        dim(Bins,[N]),
        dim(Weights,[N]),

        (for(B,1,N),
         param(Weights,Bins,N,Capacity) do
             (for(J,1,N),
             fromto(0,In,Out,Sum),
             param(Weights,Bins,B) do
                  %                        reification (0 or 1)
                  Out #= In + Weights[J] * (Bins[J] #= B)
             ),
             Sum #=< Capacity
        ).


%
% version 3: put the calculation in the fromto call (instead
%            of the do body). eval(Sum) is now needed, and the param with
%            the variables in the J loop is still needed.
%
bin_packing3(Bins, Weights, Capacity) :-
        dim(Bins,[N]),
        dim(Weights,[N]),

        (for(B,1,N),
         param(Weights,Bins,N,Capacity) do
             (for(J,1,N),
             fromto(0,In,In + Weights[J]*(Bins[J] #= B),Sum),
             param(Weights,Bins,B) do
                  true
             ),
             eval(Sum) #=< Capacity
        ).


%
% version 4: Use eval and a foreach(S, Sum)
%            
% July 2009: This version was suggested by Joachim Schimpf             
%
bin_packing4(Bins, Weights, Capacity) :-
        dim(Bins,[N]),
        dim(Weights,[N]),

        ( for(B,1,N),
          param(Weights,Bins,N,Capacity) do
              (for(J,1,N),
               foreach(S,Sum),
               param(Weights,Bins,B) do
                   S = Weights[J]*(Bins[J] #= B)
              ),
              sum(Sum) #=< Capacity
        ).



go :-
        % we let something be unknown
        problem(2,Bins, Weights,_Capacity),
        % N = 3, % if Bins is free we must set the length of it
        dim(Bins,[N]),
        Bins :: 1..N,

        dim(Weights, [N]),
        Weights :: 1..4,

        % Capacity = 5,
        Capacity :: 1..20,

        % bin_packing3(Bins,Weights,Capacity),
        bin_packing4(Bins,Weights,Capacity),
        
        term_variables([Capacity,Weights,Bins],Vars),
        % search(Vars,0,first_fail,indomain,complete,[backtrack(Backtrack)]),
        minimize(search(Vars,0,first_fail,indomain,complete,
                         [backtrack(Backtrack)]), Capacity),

        writeln(capacity:Capacity),
        writeln(bins:Bins),
        writeln(weights:Weights),
        writeln(backtracks:Backtrack),
        fail.
        

problem(1, [](3,1,3), [](4,3,1), 5).
% another example
problem(2, [](3,1,3,2,2,1,2,3), [](4,3,1,3,4,3,1,2), 5).
