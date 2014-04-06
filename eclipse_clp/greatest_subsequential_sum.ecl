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
% Note: This model is based on an idea (MiniZinc model) from Claudio Cesar de Sá.
%
%
%  Model created by Hakan Kjellerstrand, hakank@gmail.com
%  See also my ECLiPSe page: http://www.hakank.org/eclipse/
%

:-lib(ic).
:-lib(ic_global).
% :-lib(ic_search).
:-lib(branch_and_bound).

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
   N is Rand mod 100,
   ( (for(_,1,N),
      foreach(R,Vector) do
          random(R1),
          R is (R1 mod 1000)-200
     )
   ),
   writeln(vector=Vector),
   solveit(Vector),
   nl.


solveit(Vector) :-
  greatest_subsequential_sum(Vector, Begin,End,X,TotalSum),
  writeln(vector:Vector),
  writeln(x:X),
  writeln(totalSum:TotalSum),
  writeln(begin:Begin),
  writeln(end:End),
  array_list(A,Vector),
  Seq is A[Begin..End],
  writeln(seq=Seq),
  nl.

        
%
% greatest_subsequential_sum
%
greatest_subsequential_sum(Vector, Begin,End,X,TotalSum) :-

  length(Vector,N),

  % decision variables
  SizeWindow :: 1..N,
  Begin :: 1..N,
  End :: 1..N,

  length(X,N),
  X :: 0..1,
 
  % constraints
  TotalSum #= X*Vector, % scalar product

  % exacly one  SUM has fixed size then ...
  sum(X) #= SizeWindow,

  % The size of the window (Begin..End) is size_window.
  End #>= Begin,
  (End - Begin) #= SizeWindow- 1,

  (for(I,1,N),
   foreach(XI,X),
    param(Begin,End) do 
       % ((Begin #=< I) and (End #>= I) #= (XI #= 1)) % not correct
       ((Begin #=< I) * (End #>= I) #= (XI #= 1)) % this works
  ),

  TotalSumNeg #= -TotalSum,
  minimize(search(X,0,occurrence,indomain_min,complete,[]), TotalSumNeg).
  


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

