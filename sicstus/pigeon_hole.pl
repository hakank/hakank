/*

  Pigeon hole problem in SICStus Prolog.

  From
  ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
  """
  pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes 
  (at most 1 pigeon per hole). The boolean formulation uses n - m variables to 
  indicate, for each pigeon, its hole number. Obviously, there is a 
  solution iff n <= m.
  """

  Compare with the following models:
  * Comet  : http://www.hakank.org/comet/pigeon_hole.co
  * ECLiPSe: http://www.hakank.org/eclipse/pigeon_hole.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 3, % N pigeons
        M = 10, % M pigeon holes
        pigeon_hole(N,M, P),
        pretty_print(P),
        fail.



pigeon_hole(N,M, PigeonHoles) :-


        % N pigeons at M pigeon holes
        matrix(PigeonHoles,[N,M]),
        append(PigeonHoles,PigeonHolesList),
        domain(PigeonHolesList,0,1),

        % all pigeon must be placed and only at one hole
        ( foreach(Row,PigeonHoles)
        do
          sum(Row,#=,1)
        ),

        % max 1 pigeon per pigeon hole
        transpose(PigeonHoles,PigeonHolesTransposed),
        ( foreach(Column,PigeonHolesTransposed)
        do
          sum(Column,#=<,1)
        ),

        labeling([],PigeonHolesList).


pretty_print(X) :-
        ( foreach(Row,X) do
              write(Row),nl
        ),
        nl.

% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).
