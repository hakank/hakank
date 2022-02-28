/*

  Latin squares with diagonals in SWI Prolog

  Inspired by Eric Taucher's post
  "Latin squares" in the SWI-Prolog forum
  https://swi-prolog.discourse.group/t/latin-squares/5056

  Here is explaination of plain Latin squares.
  http://en.wikipedia.org/wiki/Latin_square:
  """
  A Latin square is an n X n table filled with n different symbols in
  such a way that each symbol occurs exactly once in each row and
  exactly once in each column. 
  """

  This variant also includes constraints that the two diagonals must
  be distinct.

  
  Here is a general model that count the number of solutions for N <= 6
  fairly fast.
  
  
  N  #sols  time/1
  -------------------------------------------------------------------------------
  1      1           1,972 inferences,  0.000 CPU in  0.000 seconds (100% CPU, 7287805 Lips)
  2      0           8,094 inferences,  0.001 CPU in  0.001 seconds (100% CPU, 11839322 Lips
  3      0          22,259 inferences,  0.001 CPU in  0.001 seconds (100% CPU, 15460076 Lips)
  4     48         205,097 inferences,  0.010 CPU in  0.010 seconds (100% CPU, 21095143 Lips)
  5    960       4,728,770 inferences,  0.204 CPU in  0.204 seconds (100% CPU, 23172521 Lips)
  6  92160   1,339,542,123 inferences, 59.874 CPU in 59.874 seconds (100% CPU, 22372698 Lips)

  My Picat program (http://hakank.org/picat/latin_squares_diagonals.pi)
  solves N=7 in 1 hour (it solves N=6 in 0.5s):
  N  #sols
  ----------------------
  7  862848000


  Also see: https://oeis.org/A274806
  Here are the counts from 1..8:
  1, 0, 0, 48, 960, 92160, 862848000, 300286741708800

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

/*
   The following predicates are defined in http://hakank.org/swi_prolog/hakank_utils.py
   - new_matrix/4
   - diagonal1_slice/2
   - diagonal2_slice/2
   - print_matrix/1
  
*/

%
% Show all solutions for N=4.
% 
go :-
        N = 4,
        new_matrix(N,N,1..N,X),
        latin_square_diagonals(X),
        flatten(X,Vars),
        labeling([ffc,up,bisect],Vars),
        print_matrix(X),
        fail,
        nl.
go.

%
% Find and count the number of solutions for N=6.
%
go2 :-
        N = 6,
        new_matrix(N,N,1..N,X),
        time(findall(X,(latin_square_diagonals(X),
                        flatten(X,Vars),
                        labeling([ffc,up,step],Vars)),L)),
        length(L,Len),
        writeln(len=Len),

        nl.
go2.

%
% latin_square_diagonal(X)
% 
% Ensure that X is a Latin square as well as the
% constraints that the two diagonals should be distinct.
%
latin_square_diagonals(X) :-
        maplist(all_different,X),
        
        transpose(X,XT),
        maplist(all_different,XT),
        
        diagonal1_slice(X,Slice1),
        all_different(Slice1),
        
        diagonal2_slice(X,Slice2),
        all_different(Slice2).
