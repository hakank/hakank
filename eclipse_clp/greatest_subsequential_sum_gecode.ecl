% 
% Greatest subsequential sum (Rosetta Code) in MiniZinc.
% 
% http://rosettacode.org/wiki/Greatest_subsequential_sum
% """
% Given a sequence of integers, find a continuous subsequence which maximizes 
% the sum of its elements, that is, the elements of no other single subsequence 
% add up to a value larger than this one. An empty subsequence is considered to have 
% the sum 0; thus if all elements are negative, the result must be the empty sequence.
% """

%
% See greatest_subsequential_sum.ecl.
% This model use Gecode's regular instead.
%
%
%  Model created by Hakan Kjellerstrand, hakank@gmail.com
%  See also my ECLiPSe page: http://www.hakank.org/eclipse/
%

:-lib(gfd).
% :-lib(gfd_search).
:-lib(branch_and_bound). % for minimize
:-lib(util).
:-lib(listut).

%
% Test the problems
%
go :-
   N = 7,
   (for(Problem,1,N) do
      writeln(problem:Problem), 
      problem(Problem,Vector),
      solveit(Vector),
      nl
   ),
   nl.

%
% generate and solve a random problem
%
go2 :-
   random(Rand),
   N is 1+(Rand mod 200),
   ( (for(_,1,N),
      foreach(R,Vector) do
          random(R1),
          R is (R1 mod 1000)-500
     )
   ),
   writeln(vector=Vector),
   length(Vector,Len),
   writeln(len=Len),
   solveit(Vector),
   nl.


solveit(Vector) :-
  time(greatest_subsequential_sum(Vector, X,TotalSum,Backtracks,Stats)),
  writeln(vector:Vector),
  writeln(x:X),
  writeln(totalSum:TotalSum),
  XRange #= sum(X),
  writeln(rangeLen=XRange),
  % get_range1(X,Vector,Begin,End,Range),
  % writeln([begin=Begin,end=End, range=Range]),
  get_range(X,Vector,Range),
  writeln(range=Range),
  writeln(backtracks:Backtracks),
  Stats = gfd_stats(Prop,Fail,Node,Depth,Mem),
  writeln([propagations=Prop,failures=Fail,nodes=Node,depth=Depth,memory=Mem]),
  nl.


% Extract the used range (Begin..End) from Vector
get_range(Xs,Vector,VectorRange) :-
  length(Vector,Len),
  ( for(_I,1,Len),
    foreach(X,Xs),
    foreach(V,Vector),
    fromto(VectorRange,Out,In,[]) do
        (
            X =:= 1 ->
                Out = [V|In]
        ; 
                Out = In
        )
  ).

% alternative
get_range1(X,Vector,Begin,End,VectorRange) :-
  element(Pos,X,1),
  get_integer_bounds(Pos,Begin,End),
  get_interval(Vector,Begin,End,VectorRange).
  % findall(S,(between(Begin,End,P),nth1(P,Vector,S)),VectorRange).

get_interval(X,Begin,End,Range) :- 
        once(get_interval(1,X,Begin,End,[],RangeR)),
        reverse(Range,RangeR).

get_interval(_,[],_Begin,_End,Range,Range).
get_interval(N,[X|Xs],Begin,End,Range0,Range) :-
  N >= Begin, 
  N =< End,
  Range1 = [X|Range0],
  N1 is N+1,
  get_interval(N1,Xs,Begin,End,Range1,Range).
get_interval(N,[_X|Xs],Begin,End,Range0,Range) :-
  (N < Begin; N > End),
  N1 is N+1,
  get_interval(N1,Xs,Begin,End,Range0,Range).


%
% greatest_subsequential_sum
%
greatest_subsequential_sum(Vector, X,TotalSum, Backtracks,Stats) :-

  length(Vector,N),

  length(X,N),
  X :: 0..1,
 
  % constraints
  TotalSum #= scalar_product(Vector,X,#=),

  % global contiguity constraint
  regular(X, *(0) + *(1) + *(0)),

  TotalSumNeg #= -TotalSum,
  term_variables([X,TotalSum],Vars),

  % This don't work, using gfd:search/6 or just search/6:
  % After finding the optimal value, it just hangs....
  % minimize(search(Vars,0,occurrence,indomain_min,complete,[backtrack(Backtracks)]),TotalSumNeg).
  % minimize(gfd:search(Vars,0,occurrence,indomain_min,complete,[backtrack(Backtracks)]),TotalSumNeg).

  % Ah, this works: i.e. using gfd_search:search/6
  % minimize(gfd_search:search(Vars,0,first_fail,indomain_max,complete,[backtrack(Backtracks)]),TotalSumNeg).

  % But both these works, i.e. using bb_bin/1 and restart_min/1:
  % gfd:search(Vars,0,most_constrained,indomain_max,bb_min(TotalSumNeg),[stats(Stats),backtrack(Backtracks)]).
  gfd:search(Vars,0,first_fail,indomain_min,restart_min(TotalSumNeg),[stats(Stats),backtrack(Backtracks)]).

  % Both these works as well 
  % minimize(labeling(Vars), TotalSumNeg). % built-in labeling
  % minimize(labeling2(Vars,occurrence,indomain_max), TotalSumNeg). % user defined labeling


% See http://eclipseclp.org/doc/bips/lib/gfd/select_var-5.html
labeling2(Vars, Select, Choice) :-
        (select_var(V, Vars, Rest, 0, Select) ->
             try_value(V, Choice),
             % indomain(V, Choice),
             labelling(Rest, Select, Choice)
        ;
             true
        ).



%% sum = 163, x = [1,1,1,1,1]
problem(1,[144,  5, -8,  7, 15]).

%% sum=144, x = [1,0,0,0,0]
problem(2,[144,  -145, -8,  7, 15]).

%% sum = 22,  x = [0,0,0,1,1]
problem(3,[-144,  5, -8,  7, 15]).

% some examples from http://rosettacode.org/wiki/Greatest_subsequential_sum

%% sum = 15, x: [0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0]
problem(4,[-1,-2,3,5,6,-2,-1,4,-4,2,-1]).

%% sum = 3, x = [0,0,1]
problem(5,[-1,-2, 3]).

%% sum = 19, x: [0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0]
problem(6,[7,-6,-8,5,-2,-6,7,4,8,-9,-3,2,6,-4,-6]).

%% sum = 4, x = [1]
problem(7,[4]).
