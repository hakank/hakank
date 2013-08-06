/*

  Set covering deployment in B-Prolog.

  From http://mathworld.wolfram.com/SetCoveringDeployment.html
  """
  Set covering deployment (sometimes written "set-covering deployment"
  and abbreviated SCDP for "set covering deployment problem") seeks 
  an optimal stationing of troops in a set of regions so that a 
  relatively small number of troop units can control a large 
  geographic region. ReVelle and Rosing (2000) first described 
  this in a study of Emperor Constantine the Great's mobile field 
  army placements to secure the Roman Empire.
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



% First find the optimal value (MinVal), 
% then find all the solutions with that value.
go :-
        
        write('Find the optimal solution'),nl,
        problem(Matrix),
        armies(Armies),
        write(armies:Armies),nl,
        set_covering_deployment(Matrix, Armies, MinVal,_),

        format('\nNow find all optimal solutions with MinVal ~d:\n', [MinVal]),
        findall(Assignments, 
                set_covering_deployment(Matrix, Armies, MinVal, Assignments), 
                L),
        length(L, Len),
        nl,
        writeln('All solutions:'),
        foreach(Sol in L, writeln(Sol)),
        format('\nIt was ~d solution(s)\n', [Len]).



set_covering_deployment(Matrix, Armies, MinVal, Assignments) :-

        % adjacency matrix of the cities, order N
        N @= Matrix^length,

        % first army
        length(Xs,N),
        Xs :: 0..1,

        % second army
        length(Ys,N),
        Ys :: 0..1,

        %
        % Constraint 1: There is always an army in a city (+ maybe a backup)
        %               Or rather: Is there a backup, there must be an
        %               an army
        % 
        foreach((X,Y) in (Xs,Ys), X #>= Y),

        %
        % Constraint 2: There should always be an backup army near
        % every city
        %
        foreach((X,MatRow) in (Xs,Matrix),
                X + sum([Y*M : (M,Y) in (MatRow,Ys)]) #>= 1
        ),
        

        % objective: minimize the number of armies
        MinVal #= sum([X+Y : (X,Y) in (Xs,Ys)]),

        % either search for all solutions (for the minimum value) or
        % the optimal value
        term_variables([Xs,Ys,MinVal],Vars),
        (
            ground(MinVal) 
        -> 
            labeling([],Vars)
        ;
            minof(labeling(Vars),MinVal)
        ),
        

        % convert X and Y to nicer representation
        assignments(Xs,Ys,Armies,Assignments),

        writeln(minVal:MinVal),
        writeln(x:Xs),
        writeln(y:Ys),nl,
        writeln(assigments: Assignments),
        nl,nl.


assignments(Xs,Ys,Armies,Assignments) :- 
        length(Ys,Len),
        Assignments @= [(A:Num) : (X,Y,I) in (Xs, Ys,1..Len), 
                                   [Num,A],
                                   (Num #= X +Y, 
                                    Num > 0 -> nth1(I,Armies,A)) ].


problem([[0, 1, 0, 1, 0, 0, 1, 1],
         [1, 0, 0, 1, 0, 0, 0, 0],
         [0, 0, 0, 0, 1, 1, 0, 0],
         [1, 1, 0, 0, 0, 0, 1, 0],
         [0, 0, 1, 0, 0, 1, 1, 0],
         [0, 0, 1, 0, 1, 0, 1, 1],
         [1, 0, 0, 1, 1, 1, 0, 1],
         [1, 0, 0, 0, 0, 1, 1, 0]]).

armies(['Alexandria', 'Asia Minor', 'Britain', 'Byzantium', 
        'Gaul', 'Iberia', 'Rome', 'Tunis']).
