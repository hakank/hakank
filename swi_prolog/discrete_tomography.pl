/*

  Discrete tomography in SWI Prolog

  Note: The origin of the problem is from ECLiPSe,
  but this model has been transformed in this way
     MiniZinc -> SICStus Prolog -> ECLiPSe -> B-Prolog -> Picat -> SWI Prolog
  Here is my own take at the problem.

  Problem from http://eclipse-clp.org/examples/tomo.ecl.txt
  """
  This is a little "tomography" problem, taken from an old issue
  of Scientific American.
 
  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:
 
  ?- go.
     0 0 7 1 6 3 4 5 2 7 0 0
  0                         
  0                         
  8      * * * * * * * *    
  2      *             *    
  6      *   * * * *   *    
  4      *   *     *   *    
  5      *   *   * *   *    
  3      *   *         *    
  7      *   * * * * * *    
  0                         
  0                         
 
 
  Eclipse solution by Joachim Schimpf, IC-Parc
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        between(1,4,P),
        findall(X,discrete_tomography(P,X),L),
        maplist(pretty_print,L),
        fail,
        nl.

go.


discrete_tomography(P,X) :-

        problem(P,RowSums,ColSums),
        format("\nProblem ~d:\n",[P]),

        length(RowSums,Rows),
        length(ColSums,Cols),

        new_matrix(Rows,Cols,0..1, X),
    
        %% check row/col sums
        maplist(check_sums,X,RowSums),
        transpose(X,XT),
        maplist(check_sums,XT,ColSums),

        flatten(X,Vars),
        label(Vars).

check_sums(X,Sum) :-
        sum(X,#=,Sum).

%%
%% Convert to a nicer picture
%% replace 0 -> ' ', 1 -> '#'
%%
pretty_print(X) :-
        Convert = [[0,' '],[1,'#']],
        maplist(pretty_print_row(Convert),X),
        nl.

pretty_print_row(Convert,X) :-
        convert_row(X,Convert,[],X2),
        atom_string(X2,S),
        writeln(S).

convert_row([],_Convert,S,S).
convert_row([C|Cs],Convert, S0,[C2|S]) :-
        memberchk([C,C2],Convert),
        convert_row(Cs,Convert,S0,S).


%
% The three first problems are from the ECLiPSe model:
%
% The above stated problem
problem(1, R, S) :- 
        R = [0,0,8,2,6,4,5,3,7,0,0],  % row sums
        S = [0,0,7,1,6,3,4,5,2,7,0,0]. % column sums


problem(2, R, S) :-
        R = [10,4,8,5,6],
        S = [5,3,4,0,5,0,5,2,2,0,1,5,1].


% This give three slightly different solutions.
problem(3, R, S) :- 
        R = [11,5,4],
        S = [3,2,3,1,1,1,1,2,3,2,1].


% This is my own problem.
problem(4, R, S) :- 
        R = [0,2,2,2,2,2,8,8,4,4,4,4,4,0],
        S = [0,0,0,12,12,2,2,2,2,7,7,0,0,0].
