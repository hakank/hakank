/*

  Fill-a-pix problem in SWI Prolog

  From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
  """
  Each puzzle consists of a grid containing clues in various places. The 
  object is to reveal a hidden picture by painting the squares around each 
  clue so that the number of painted squares, including the square with 
  the clue, matches the value of the clue. 
  """
 
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  """
  Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated 
  picture hidden inside. Using logic alone, the solver determines which 
  squares are painted and which should remain empty until the hidden picture 
  is completely exposed.
  """
  
  Fill-a-pix History:
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
    problem(1,Problem),
    fill_a_pix(Problem).

go2 :-
    between(1,3,P),
    format("~nProblem ~w~n",P),
    problem(P,Problem),
    fill_a_pix(Problem),
    fail,
    nl.

go2.


fill_a_pix(Problem) :-

    length(Problem,N),
    new_matrix(N,N,0..1, X),

    %%
    %% Sum all the neighbours
    %%
    findall([ProblemIJ,IAJB],
            (
             between(1,N,I),between(1,N,J),
             matrix_element(Problem,I,J,ProblemIJ),
             ground(ProblemIJ),
             findall([IA,JB],
                     (
                     between(-1,1,A),between(-1,1,B),
                     IA #= I+A, JB #= J+B,
                     IA #> 0, JB #> 0,
                      IA #=< N, JB #=< N
                     ),
                     IAJB
                    )
            ),
            IJs),
    maplist(sum_neighbours(X),IJs),
    
    flatten(X,Vars),
    label(Vars),

    pretty_print(X),
    nl.

sum_neighbours(X,[P,IJs]) :-
        extract_from_indices2d(IJs,X,Xs),
        sum(Xs,#=,P).


%%
%% Convert to a nicer picture
%% replace 0 -> ' ', 1 -> '#'
%%
pretty_print(X) :-
        Convert = [[0,' '],[1,'#']],
        maplist(pretty_print_row(Convert),X).

pretty_print_row(Convert,X) :-
        convert_row(X,Convert,[],X2),
        atom_string(X2,S),
        writeln(S).

convert_row([],_Convert,S,S).
convert_row([C|Cs],Convert, S0,[C2|S]) :-
        memberchk([C,C2],Convert),
        convert_row(Cs,Convert,S0,S).
        


% Puzzle 1 from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
% 
problem(1, P) :- 
        P = [[_,_,_,_,_,_,_,_,0,_],
             [_,8,8,_,2,_,0,_,_,_],
             [5,_,8,_,_,_,_,_,_,_],
             [_,_,_,_,_,2,_,_,_,2],
             [1,_,_,_,4,5,6,_,_,_],
             [_,0,_,_,_,7,9,_,_,6],
             [_,_,_,6,_,_,9,_,_,6],
             [_,_,6,6,8,7,8,7,_,5],
             [_,4,_,6,6,6,_,6,_,4],
             [_,_,_,_,_,_,3,_,_,_]].





% Puzzle 2 from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
% 
problem(2, P) :- 
        P = [[0,_,_,_,_,_,3,4,_,3],
             [_,_,_,4,_,_,_,7,_,_],
             [_,_,5,_,2,2,_,4,_,3],
             [4,_,6,6,_,2,_,_,_,_],
             [_,_,_,_,3,3,_,_,3,_],
             [_,_,8,_,_,4,_,_,_,_],
             [_,9,_,7,_,_,_,_,5,_],
             [_,_,_,7,5,_,_,3,3,0],
             [_,_,_,_,_,_,_,_,_,_],
             [4,4,_,_,2,3,3,4,3,_]].


% Puzzle from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
%
% Code: 030.15x15
% ID: 03090000000
% 
problem(3, P) :- 
        P = [[_,5,_,6,_,_,_,_,_,_,6,_,_,_,_],
             [_,_,7,6,_,4,_,_,4,_,_,8,9,_,5],
             [5,_,_,5,_,5,_,3,_,6,_,7,_,_,6],
             [4,_,2,_,4,_,4,_,3,_,2,_,_,9,_],
             [_,_,_,5,_,4,_,3,_,4,_,4,5,_,6],
             [_,4,3,3,4,_,_,_,4,_,2,_,_,_,_],
             [_,_,_,_,_,_,_,_,_,5,_,_,_,4,_],
             [3,_,3,_,_,3,_,_,_,5,_,4,4,_,_],
             [_,_,_,4,3,_,3,3,_,_,5,7,6,_,_],
             [4,_,_,_,2,_,3,3,2,_,8,9,_,5,_],
             [_,_,3,_,_,_,_,5,_,_,7,_,8,_,_],
             [4,_,_,3,2,_,_,_,_,_,7,_,_,6,_],
             [_,_,4,_,5,4,4,_,_,9,6,_,_,_,_],
             [_,3,5,7,_,6,_,_,_,_,_,_,7,_,_],
             [_,_,4,6,6,_,_,_,6,5,_,_,_,4,_]].
