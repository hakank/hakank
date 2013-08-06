/*

  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in B-Prolog.


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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



%
% There are 8 solutions: all states that Agatha killed herself.
%
go :- 
        % collect all possible solutions, and collect them
        setof([killer:Killer], who_killed_agatha(Killer), L),
        writeln(L),
        findall([killer:Killer2], who_killed_agatha(Killer2), L2),
        writeln(L2).


who_killed_agatha(Killer) :-

        % Setup
        N = 3,

        Agatha = 1,
        Butler = 2,
        Charles = 3,

        Killer :: 1..3,

        %
        % define the Hates and Richer matrices
        %
        new_array(Hates,[N,N]),
        array_to_list(Hates,HatesVar),
        HatesVar :: 0..1,

        new_array(Richer, [N,N]),
        array_to_list(Richer,RicherVar),
        RicherVar :: 0..1,

        %
        % The constraints
        %

        %
        % Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
        % are the only ones to live there. 
        %

        % * A killer always hates, and is no richer than his victim. 
        
        % Note, the following is not supported in B-Prolog:
        %
        %   Hates[Killer,Victim] #= 1,
        %   Richer[Killer, Victim] #= 0,
        %
        % This works, though:
        foreach(I in 1..N,
                (
                    Killer #= I #=> Hates[I, Agatha] #= 1,
                    Killer #= I #=> Richer[I, Agatha] #= 0
                )
               ),

        % * Define the concept of richer: no one is richer than him-/herself
        foreach(I in 1..N, Richer[I,I] #= 0),
        
        % (contd...) if i is richer than j then j is not richer than i
        foreach(I in 1..N, J in 1..N,
                (
                    I \= J 
                ->
                    Richer[I,J] #= 1 #=> Richer[J,I] #= 0,
                    Richer[J,I] #= 0 #=> Richer[I,J] #= 1
                ; 
                    true
                )
        ),

        % * Charles hates no one that Agatha hates. 
        foreach(I in 1..N, Hates[Agatha, I] #= 1 #=> Hates[Charles, I] #= 0),

        % * Agatha hates everybody except the butler. 
        Hates[Agatha, Butler]  #= 0,
        Hates[Agatha, Charles] #= 1,
        Hates[Agatha, Agatha]  #= 1,


        % * The butler hates everyone not richer than Aunt Agatha. 
        foreach(I in 1 ..N,
              Richer[I, Agatha] #= 0 #=> Hates[Butler, I] #= 1
        ),

        % * The butler hates everyone whom Agatha hates. 
        foreach(I in 1..N, Hates[Agatha, I] #= 1 #=> Hates[Butler, I] #= 1),

        % * No one hates every one.
        foreach(I in 1..N, sum([Hates[I,J] : J in 1..N]) #=< 2),

        % * Who killed Agatha?


        term_variables([Killer,HatesVar,RicherVar], Vars),
        labeling(Vars).

        %writeln(victim:Victim),
        %writeln(killer:Killer),
        %writeln(hates:Hates),
        %writeln(richer:Richer), fail
