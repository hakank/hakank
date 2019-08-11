/*

  Curious set of integers in SWI Prolog

  Martin Gardner (February 1967):
  """
  The integers 1,4,9, and 120 form a set with a remarkable property:
  the product of any two integers is one less than a perfect square. 
  Find a fifth number that can be added to the set without destroying 
  this property.
  """

  Solution: The number is 0.
 
  There are however other sets of five numbers with this property.
  Here are the one in the range of 0.10000:

  [0, 1, 3, 8, 120]
  [0, 1, 3, 120, 1680]
  [0, 1, 8, 15, 528]
  [0, 1, 8, 120, 4095]
  [0, 1, 15, 24, 1520]
  [0, 1, 24, 35, 3480]
  [0, 1, 35, 48, 6888]
  [0, 2, 4, 12, 420]
  [0, 2, 12, 24, 2380]
  [0, 2, 24, 40, 7812]
  [0, 3, 5, 16, 1008]
  [0, 3, 8, 21, 2080]
  [0, 3, 16, 33, 6440]
  [0, 4, 6, 20, 1980]
  [0, 4, 12, 30, 5852]
  [0, 5, 7, 24, 3432]
  [0, 6, 8, 28, 5460]
  [0, 7, 9, 32, 8160]


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        % time(curious(X,ff,enum)),
        time(curious(X,[ffc])),        
        writeln(X),
        fail,
        nl.

go2 :-
        bench_labelings,
        nl.

% go2 :- 
%    selection(VariableSelect),
%    choice(ValueSelect),
%    foreach(VarSel in VariableSelect, ValSel in ValueSelect)
%         writeln([VarSel, ValSel]),
%         time2(curious(X,VarSel, ValSel)),
%         writeln(X),
%         nl,nl
%    end.


curious(X,Label) :-

   N = 5, 
   length(X,N),
   DomMax = 10000,
   X ins 0..DomMax,

   all_different(X),
   increasing(X),

   findall([I,J],
           (between(1,N,I),
            I1 #= I-1,
            between(1,I1,J)
           ),
           IJs
          ),
   maplist(check_numbers(X,DomMax),IJs),
   
   labeling(Label,X).

check_numbers(X,DomMax,[I,J]) :-
        P in 0..DomMax,
        element(I,X,XI),
        element(J,X,XJ),
        PP1 #= P*P-1,
        PP1 #= XI*XJ.
        

%%
%% Benchmark for first solution.
%% Note: I skipped down entirely here.
%%
%% [leftmost,up,step], Time: 0.090055s
%% [leftmost,up,enum], Time: 0.080868s
%% [leftmost,up,bisect], Time: 0.119519s
%% [ff,up,step], Time: 0.079620s
%% [ff,up,enum], Time: 0.080162s
%% [ff,up,bisect], Time: 0.118843s
%% [ffc,up,step], Time: 0.081971s
%% [ffc,up,enum], Time: 0.078830s
%% [ffc,up,bisect], Time: 0.115192s
%% [min,up,step], Time: 0.080990s
%% [min,up,enum], Time: 0.082508s
%% [min,up,bisect], Time: 0.120426s
%% [max,up,step], Time: 0.080001s
%% [max,up,enum], Time: 0.080085s
%% [max,up,bisect], Time: 13.301223s
%%
bench_labelings :-

        VarSelect = [leftmost,ff,ffc,min,max],
        ValSelect = [up], % [up,down], % Skipping down entirely
        BranchSelect = [step,enum,bisect],
        member(Var,VarSelect),
        member(Val,ValSelect),
        member(Branch, BranchSelect),
        Label = [Var,Val,Branch],
        %% Don't forget once/1 here.
        time2(once(curious(_X,Label)),Time), 
        format("~w, Time: ~fs~n", [Label, Time]),       
        fail,
        nl.

bench_labelings.