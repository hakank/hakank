/*

  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in ECLiPSe.
 
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
  * JaCoP : http://www.hakank.org/JaCoP/WhoKilledAgatha.java
  * JaCoP : http://www.hakank.org/JaCoP/WhoKilledAgatha_element.java
  * Choco: http://www.hakank.org/choco/WhoKilledAgatha.java
  * Choco: http://www.hakank.org/choco/WhoKilledAgatha_element.java
  * Comet: http://www.hakank.org/comet/who_killed_agatha.co
  * Gecode: http://www.hakank.org/gecode/who_killed_agatha.cpp


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
%  :-lib(ic_search).
:-lib(propia).


%
% There are 8 solutions: all states that Agatha killed herself.
%
go :- 
        % collect all possible solutions, and collect them
        % setof([killer:Killer,victim:Victim], who_killed_agatha(Killer,Victim), L),
        findall([killer:Killer,victim:Victim], who_killed_agatha(Killer,Victim), L),
        writeln(L).


who_killed_agatha(Killer, Victim) :-

        %
        % Setup
        %
        N = 3,

        Agatha = 1,
        Butler = 2,
        Charles = 3,

        Killer #:: 1..3,
        Victim #:: 1..3,

        %
        % define the Hates and Richer matrices
        %
        dim(Hates, [N,N]),
        Hates :: 0..1,

        dim(Richer, [N,N]),
        Richer :: 0..1,

        %
        % The constraints
        %

        %
        % Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
        % are the only ones to live there. 
        %

        % A killer always hates, and is no richer than his victim. 
        
        % Note, the following syntax is not directly supported in ECLiPSe:
        %
        %   Hates[Killer,Victim] #= 1,
        %   Richer[Killer, Victim] #= 0,
        %
        %
        % This works, though.
        % ( for(I,1,N), param(Killer,Victim,Hates,Richer) do
        %      Killer #= I => Hates[I, Victim] #= 1,
        %      Killer #= I => Richer[I, Victim] #= 0
        % ),

        % Using suspend is better, and probably faster.
        % suspend(Hates[Killer,Victim] #= 1, 2, Killer->inst),
        % suspend(Richer[Killer, Victim] #= 0, 2, Killer->inst),

        % This works in ECLiPSe 6.0 #96
        Hates[Killer,Victim] #= 1,
        Richer[Killer, Victim] #= 0,


        % define the concept of richer: no one is richer than him-/herself
        (for(I,1,N), param(Richer) do Richer[I,I] #= 0),
        
        % (contd...) if i is richer than j then j is not richer than i
        ( for(I,1,N) * for(J,1,N), param(Richer) do 
              I \= J 
        ->
          Richer[I,J] #= 1 => Richer[J,I] #= 0,
          Richer[J,I] #= 0 => Richer[I,J] #= 1
        ; 
          true
        ),

        % Charles hates noone that Agatha hates. 
        ( for(I,1,N), param(Hates,Agatha,Charles) do
              Hates[Agatha, I] #= 1 => Hates[Charles, I] #= 0
        ),

        % Agatha hates everybody except the butler. 
        Hates[Agatha, Butler]  #= 0,
        Hates[Agatha, Charles] #= 1,
        Hates[Agatha, Agatha]  #= 1,


        % The butler hates everyone not richer than Aunt Agatha. 
        ( for(I,1,N), param(Hates,Richer,Butler,Agatha) do
              Richer[I, Agatha] #= 0 => Hates[Butler, I] #= 1
        ),

        % The butler hates everyone whom Agatha hates. 
        ( for(I,1,N), param(Hates,Butler,Agatha) do
              Hates[Agatha, I] #= 1 => Hates[Butler, I] #= 1
        ),

        % Noone hates every one.
        ( for(I,1,N), param(Hates,N) do
              HatesSum #= sum(Hates[I,1..N]),
              HatesSum #=< 2
        ),

        % Who killed Agatha?
        Victim #= Agatha,
         

        term_variables([Killer,Victim,Hates,Richer], Vars),
        % labeling(Vars).
        search(Vars,0,first_fail,indomain,complete,[]).

        %writeln(victim:Victim),
        %writeln(killer:Killer),
        %writeln(hates:Hates),
        %writeln(richer:Richer), fail
