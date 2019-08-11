/*

  Magic squares in SWI Prolog

  https://en.wikipedia.org/wiki/Magic_square
  """
  In recreational mathematics and combinatorial design, a magic square[1] is a n × n
  square grid (where n is the number of cells on each side) filled with distinct positive
  integers in the range 1, 2, ...,n^2 such that each cell contains a different integer and
  the sum of the integers in each row, column and diagonal is equal.
  The sum is called the magic constant or magic sum of the magic square. A square grid with
  n cells on each side is said to have order n.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% Simple run
%
go :-
        N = 5,
        time(once(magic(N,Square))),
        print_matrix(Square),
        nl.

%%
%% All solutions.
%%
go2 :-
        N = 4,
        findall(Square,magic(N,Square),L),
        length(L,Len),
        format("Len: ~d~n",Len),
        nl.

%%
%% Time for first solution
%%
go3 :-
        between(1,6,N),
        time2(once(magic(N,Square)),Time),
        format("~d: ~f~n", [N,Time]),
        print_matrix(Square),
        nl,
        fail,
        nl.

go3.

magic(N,Square) :-

  format("N: ~d\n", [N]),
  NN #= N*N,
  Sum #= N*(NN+1)//2,% magical sum
  format("Sum = ~d\n", [Sum]),

  new_matrix(N,N,1..NN,Square),
  flatten(Square,Vars),
 
  all_distinct(Vars),

  maplist(sums(Sum),Square),
  transpose(Square,SquareT),
  maplist(sums(Sum),SquareT),

  
  % diagonal sums
  findall([I,I], between(1,N,I), IJs),
  extract_from_indices2d(IJs,Square,[], Diagonal1),
  sum(Diagonal1,#=, Sum),

  % diagonal 2 sum
  findall([I,NI1], (between(1,N,I), NI1 #= N-I+1), IJs2),
  extract_from_indices2d(IJs2,Square,[], Diagonal2),
  sum(Diagonal2,#=, Sum),

  %% Symmetry breaking
  matrix_element(Square,1,1,S11),
  matrix_element(Square,N,N,SNN),
  matrix_element(Square,N,1,SN1),
  matrix_element(Square,1,N,S1N),
  
  matrix_element(Square,1,2,S12),
  matrix_element(Square,2,1,S21),  

  % Symmetry breaking
  % S11 #< S1N,
  % S11 #< SN1,  
  % S11 #< SNN,
  % S1N #< SN1,

  %% Symmetry breaking, Frenicle form
  min_list_clp([S11,S1N,SN1,SNN], S11),
  S12 #< S21,
  
  labeling([ffc,up,enum],Vars).

%%
%% sum a row/column (for maplist/2)
%%
sums(Sum,L) :-
        sum(L,#=,Sum).

