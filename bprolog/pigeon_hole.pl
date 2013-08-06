/*

  Pigeon hole problem in B-Prolog.

  From
  ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
  """
  pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes 
  (at most 1 pigeon per hole). The boolean formulation uses n - m variables to 
  indicate, for each pigeon, its hole number. Obviously, there is a 
  solution iff n <= m.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        N = 3,  % N pigeons
        M = 10, % M pigeon holes
        findall(P, pigeon_hole(N,M, P),L),
        foreach(Sol in L, pretty_print(Sol)),
        length(L,Len),
        format("It was ~d solutions.\n", [Len]).


% This is an impossible problem (M < N)
go2 :-
        N = 5,  % N pigeons
        M = 4, % M pigeon holes
        findall(P, pigeon_hole(N,M, P),L),
        foreach(Sol in L, pretty_print(Sol)),
        length(L,Len),
        format("It was ~d solutions.\n", [Len]).



pigeon_hole(N,M, PigeonHoles) :-


        % N pigeons at M pigeon holes
        new_array(PigeonHoles,[N,M]),
        array_to_list(PigeonHoles,Vars),
        Vars :: 0..1,
        
        % all pigeon must be placed and only at one hole
        Rows @= PigeonHoles^rows,

        foreach(Row in Rows, sum(Row) #=1),

        % max 1 pigeon per pigeon hole
        Columns @= PigeonHoles^columns,
        foreach(Column in Columns, sum(Column) #=< 1),

        labeling([],Vars).


pretty_print(X) :-
        Rows @= X^rows,
        foreach(Row in Rows, writeln(Row)), nl.

