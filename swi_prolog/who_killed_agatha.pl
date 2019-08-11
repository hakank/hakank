/*

  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in SWI Prolog

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

  Here is a more detailed explanation (describing the MiniZinc and Picat models):
  * Decision Model "Who killed Agatha?"
   http://hakank.org/minizinc/who_killed_agatha_dmcommunity_challenge.html


  Note: This SWI Prolog model is ported from my Picat model (http://hakank.org/picat/who_killed_agatha.pi)
  Since SWI Prolog don't support loops or list comprehensions (AFAIK), some of the original Picat code is
  kept to help understand this model. Oh, and I'm sure that there's other approaches that is not
  that convoluted, but I wanted to implement the same general approach to this problem.

  This ("the same general approach") means that we - as expected - get 8 solutions, all indicating
  that Agatha is the killer, which is shown in go/0.

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


%
% There are 8 solutions: all states that Agatha killed herself.
%
go :- 
        % collect all possible solutions
        findall(Killer, who_killed_agatha(Killer), L),
        length(L,Len),        
        writeln(killer=L),
        writeln(len=Len).


who_killed_agatha(Killer) :-
        % Setup
        N = 3,
        Agatha = 1,
        Butler = 2,
        Charles = 3,
        People = [Agatha,Butler,Charles],
        
        Killer in 1..N,

        %
        % define the Hates and Richer matrices
        %
        new_matrix(N,N, 0..1, Hates),    
        new_matrix(N,N, 0..1, Richer),
    
        % 
        % The constraints
        % 

        %
        % Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
        % are the only ones to live there. 
        %

        % * A killer always hates, and is no richer than his victim. 
        check1(People, Agatha, Killer, Hates, Richer),
    
        % * Define the concept of richer: no one is richer than him-/herself
        matrix_element(Richer,Agatha,Agatha,0),
        matrix_element(Richer,Butler,Butler,0),
        matrix_element(Richer,Charles,Charles,0),

    
        % (contd...) if i is richer than j then j is not richer than i
        % foreach(I in 1..N, J in 1..N, I != J)
        %    Richer[I,J] #= 1 #=> Richer[J,I] #= 0,
        %    Richer[J,I] #= 0 #=> Richer[I,J] #= 1
        % end,
        check2(People,Richer),

        % * Charles hates no one that Agatha hates. 
        % foreach(I in 1..N) Hates[Agatha, I] #= 1 #=> Hates[Charles, I] #= 0 end,
        check3(People, Agatha, Charles, Hates),
    
        % % * Agatha hates everybody except the butler. 
        matrix_element(Hates,Agatha,Butler,0),
        matrix_element(Hates,Agatha,Charles,1),
        matrix_element(Hates,Agatha,Agatha,1),
        
        % * The butler hates everyone not richer than Aunt Agatha. 
        check4(People,Agatha,Butler,Richer,Hates),

        % * The butler hates everyone whom Agatha hates. 
        check5(People,Agatha,Butler,Hates),
   
        % * No one hates every one.
        check6(People,Hates),
    
        % * A killer always hates, and is no richer than his victim. 
        matrix_element(Hates,Killer,Agatha,1),
        matrix_element(Richer,Killer,Agatha,0),

        % * Who killed Agatha?

        append(Hates,Richer,Vars1),
        append(Vars1,[Killer],Vars2),    
        flatten(Vars2,Vars),
        labeling([ffc, bisect],Vars).


% * A killer always hates, and is no richer than his victim. 
%    
%    foreach(I in 1..N)
%       Killer #= I #=> Hates[I, Agatha] #= 1,
%       Killer #= I #=> Richer[I, Agatha] #= 0
%    end,
%
check1([], _Agatha, _Killer, _Hates, _Richer).
check1([I|L], Agatha, Killer, Hates, Richer) :-
        matrix_element(Hates,I,Agatha,HatesA),
        matrix_element(Richer,I,Agatha,RicherA),
        Killer #= I #==> HatesA #= 1,
        Killer #= I #==> RicherA #= 0,
        check1(L, Agatha, Killer, Hates, Richer).

% % (contd...) if i is richer than j then j is not richer than i
% foreach(I in 1..N, J in 1..N, I != J)
%    Richer[I,J] #= 1 #=> Richer[J,I] #= 0,
%    Richer[J,I] #= 0 #=> Richer[I,J] #= 1
% end,
check2(L,Richer) :-
        findall([I,J], (member(I,L),member(J,L), I \= J),L1),
        check2_(L1,Richer).
check2_([],_Richer).
check2_([[I,J]|Ls], Richer) :-
        matrix_element(Richer,I,J,RIJ),
        matrix_element(Richer,J,I,RJI),
        RIJ #= 1 #==> RJI #= 0,
        RJI #= 0 #==> RIJ #= 1,
        check2_(Ls,Richer).
       
        

% * Charles hates no one that Agatha hates. 
% foreach(I in 1..N) Hates[Agatha, I] #= 1 #=> Hates[Charles, I] #= 0 end,
check3([], _Agatha, _Charles, _Hates).
check3([I|L], Agatha, Charles, Hates) :-
        matrix_element(Hates,Agatha,I,HAI),
        matrix_element(Hates,Charles,I,HCI),
        HAI #= 1 #==> HCI #= 0,
        check3(L, Agatha, Charles, Hates).
        
% * The butler hates everyone not richer than Aunt Agatha. 
% foreach(I in 1 ..N)
%   Richer[I, Agatha] #= 0 #=> Hates[Butler, I] #= 1
% end,
check4([],_Agatha,_Butler,_Richer,_Hates).
check4([I|L],Agatha,Butler,Richer,Hates) :-
        matrix_element(Richer,I,Agatha,RIA),
        matrix_element(Hates,Butler,I,HBI),
        RIA #= 0 #==> HBI #= 1,
        check4(L,Agatha,Butler,Richer,Hates).
        
% * The butler hates everyone whom Agatha hates. 
% foreach(I in 1..N) Hates[Agatha, I] #= 1 #=> Hates[Butler, I] #= 1 end,
check5([],_Agatha,_Butler,_Hates).
check5([I|L],Agatha,Butler,Hates) :-
        matrix_element(Hates,Agatha,I,HAI),
        matrix_element(Hates,Butler,I,HBI),
        HAI #= 1 #==> HBI #= 1,
        check5(L,Agatha,Butler,Hates).

% * No one hates every one.
% foreach(I in 1..N) sum([Hates[I,J] : J in 1..N]) #=< 2 end,
check6([],_Hates).
check6([I|L],Hates) :-
        %% This don't work.
        % People = [1,2,3],
        % findall(HIJ,(member(J,People), matrix_element(Hates,I,J,HIJ)),S),
        % sum(S, #=<, 2),
        % writeln(s=S),

        %% This works:
        matrix_element(Hates,I,1,HI1),
        matrix_element(Hates,I,2,HI2),
        matrix_element(Hates,I,3,HI3),
        HI1 + HI2 + HI3 #=< 2,
        check6(L,Hates).
