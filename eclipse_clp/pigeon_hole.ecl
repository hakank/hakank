/*

  Pigeon hole problem in ECLiPSe.

  ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
  """
  pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes 
  (at most 1 pigeon per hole). The boolean formulation uses n - m variables to 
  indicate, for each pigeon, its hole number. Obviously, there is a 
  solution iff n <= m.
  """

  Compare with the Comet model http://www.hakank.org/comet/pigeon_hole.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).

go :-
        N = 3, % N pigeons
        M = 10, % M pigeon holes
        pigeon_hole(N,M, P),
        pretty_print(P),
        fail.



pigeon_hole(N,M, PigeonHoles) :-


        % N pigeons at M pigeon holes
        dim(PigeonHoles,[N,M]),
        PigeonHoles::0..1,

        % max 1 pigeon per pigeon hole
        ( for(J,1,M), param(PigeonHoles,N) do
              sum(PigeonHoles[1..N,J]) #=< 1
        ),

        % all pigeon must be placed and only at one hole
        ( for(I, 1,N),
          param(PigeonHoles,M) do
              sum(PigeonHoles[I,1..M]) #= 1
        ),

        term_variables(PigeonHoles,Vars),
        labeling(Vars).



pretty_print(X) :-
        dim(X, [N,M]),
        ( for(I, 1, N), param(X, M) do
            ( for(J, 1, M), param(X, I) do
                XX is X[I,J],
                printf("%2d", XX),
                write(" ")
            ),
            nl
        ),nl.        
