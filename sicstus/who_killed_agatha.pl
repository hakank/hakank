/*

  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in
  SICStus Prolog.

  This is a standard benchmark for theorem proving.
 
  http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
  """ 
  Someone in Dreadsbury Mansion killed Aunt Agatha. 
  Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
  are the only ones to live there. A killer always hates, and is no 
  richer than his victim. Charles hates noone that Agatha hates. Agatha 
  hates everybody except the butler. The butler hates everyone not richer 
  than Aunt Agatha. The butler hates everyone whom Agatha hates. 
  Noone hates everyone. Who killed Agatha? 
  """

  Originally from F. J. Pelletier: 
  Seventy-five problems for testing automatic theorem provers. 
  Journal of Automated Reasoning, 2: 191 216, 1986.
  http://www.sfu.ca/~jeffpell/papers/75ATPproblems86.pdf


  I have blogged about the problem here:
  * "Learning constraint programming - part II: Modeling with the Element constraint"
    http://www.hakank.org/constraint_programming_blog/2009/05/learning_constraint_programmin.html
  * "Learning Constraint Programming IV: Logical constraints: Who killed Agatha? revisited"
  http://www.hakank.org/constraint_programming_blog/2009/05/learning_constraint_programmin_3.html


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/who_killed_agatha.mzn
  * JaCoP   : http://www.hakank.org/JaCoP/WhoKilledAgatha.java
  * JaCoP   : http://www.hakank.org/JaCoP/WhoKilledAgatha_element.java
  * Choco   : http://www.hakank.org/choco/WhoKilledAgatha.java
  * Choco   : http://www.hakank.org/choco/WhoKilledAgatha_element.java
  * Comet   : http://www.hakank.org/comet/who_killed_agatha.co
  * Gecode  : http://www.hakank.org/gecode/who_killed_agatha.cpp
  * ECLiPSe : http://www.hakank.org/eclipse/who_killed_agatha.cpp
  * Tailor/Essence': http://www.hakank.org/tailor/who_killed_agatha.eprime

  This model is based on the ECLiPSe model and keeps the matrix 
  representation of the problem.


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% There are 8 solutions: all states that Agatha killed herself.
%
go :- 
        % collect all possible solutions, and collect them
        findall([killer:Killer,victim:Victim], who_killed_agatha(Killer,Victim), L),
        write(L),nl,
        fd_statistics.

matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).



who_killed_agatha(Killer, Victim) :-

        % Setup
        N = 3,

        Agatha = 1,
        Butler = 2,
        Charles = 3,

        domain([Killer,Victim],1,3),

        % define the Hates and Richer matrices
        matrix(Hates,[N,N]),
        append(Hates,HatesList),
        domain(HatesList, 0,1),
        
        matrix(Richer,[N,N]),
        append(Richer, RicherList),
        domain(RicherList, 0, 1),

        %
        % The constraints

        %
        % Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
        % are the only ones to live there. 
        %

        % A killer always hates, and is no richer than his victim. 
        matrix_element(Hates,Killer,Victim,1),
        matrix_element(Richer,Killer,Victim,0),
        

        % define the concept of richer: no one is richer than him-/herself
        ( for(I,1,N), 
          param(Richer) do 
             matrix_element(Richer,I,I,0)
        ),

        % (contd...) if i is richer than j then j is not richer than i
        ( for(I,1,N),
          param(Richer,N) do 
              ( for(J,1,N), param(Richer,I) do 
                    I \= J 
              ->
                matrix_element(Richer,I,J,RIJ),
                matrix_element(Richer,J,I,RJI),
                RIJ #= 1 #=> RJI #= 0,
                RJI #= 0 #=> RIJ #= 1
              ; 
                true
              )
        ),

        
        % Charles hates noone that Agatha hates. 
        ( for(I,1,N), 
          param(Hates,Agatha,Charles) do
              matrix_element(Hates,Agatha,I,HAI),
              matrix_element(Hates,Charles,I,HCI),
              HAI #= 1 #=> HCI #= 0
        ),

        % Agatha hates everybody except the butler. 
        matrix_element(Hates, Agatha, Butler, 0),
        matrix_element(Hates, Agatha, Charles, 1),
        matrix_element(Hates, Agatha, Agatha, 1),

        % The butler hates everyone not richer than Aunt Agatha. 
        ( for(I,1,N), 
          param(Hates,Richer,Butler,Agatha) do
              matrix_element(Richer,I,Agatha,RIA),
              matrix_element(Hates,Butler,I,HBI),
              RIA #= 0 #=> HBI #= 1
         ),
        
        % The butler hates everyone whom Agatha hates. 
        ( for(I,1,N), param(Hates,Butler,Agatha) do
               matrix_element(Hates,Agatha,I,HIA),
               matrix_element(Hates, Butler,I, HBI),
               HIA #= 1 #=> HBI #= 1
         ),

        % Noone hates everyone.
        ( foreach(H, Hates), 
          param(Hates,N) do
              sum(H,#=<, 2)
        ),

        % Who killed Agatha?
        Victim #= Agatha,

        append([HatesList,RicherList],Vars),
        labeling([ff,down], Vars).
