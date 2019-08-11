/*

  Global constraint inverse/1 and inverse/2 in SWI Prolog

  From Global Constraint Catalog
  http://www.emn.fr/z-info/sdemasse/gccat/Cinverse.html
  """
  inverse(NODES)

  Enforce each vertex of a digraph to have exactly one predecessor and 
  one successor. In addition the following two statements are equivalent:
    - The successor of the ith node is the jth node.
    - The predecessor of the jth node is the ith node.
  """

  There are two inverse constraints:
  
  * inverse/1: The constraint described above is inverse/1 ("self-assignment").
  
    This means that for each element X[I] either 
       - X[I] = I
       or
       - X[I] = J <=> X[J] = I

    Example N=4:

      [1,2,3,4]
      [1,2,4,3]    (X[3]=4, X[4]=3)
      [1,3,2,4]    (X[2]=3, X[3]=2)
      [1,4,3,2] 
      [2,1,3,4]
      [2,1,4,3]    (X[1]=2,X[2]=1, X[3]=4,X[4]=3)
      [3,2,1,4]
      [3,4,1,2]
      [4,2,3,1]
      [4,3,2,1]

  * The other version is inverse/2, a.k.a. "assignment/2".
    We have two lists, L1 and L1. For each element I and J in L1 and L2:
       X[I] = J <=> Y[J] = I

    For N=4, there are 24 solutions:
  
     [[1,2,3,4],[1,2,3,4]]
     [[1,2,4,3],[1,2,4,3]]
     [[1,3,2,4],[1,3,2,4]]
     [[1,3,4,2],[1,4,2,3]]
     [[1,4,2,3],[1,3,4,2]]
     [[1,4,3,2],[1,4,3,2]]
     [[2,1,3,4],[2,1,3,4]]
     [[2,1,4,3],[2,1,4,3]]
     [[2,3,1,4],[3,1,2,4]]
     [[2,3,4,1],[4,1,2,3]]
     [[2,4,1,3],[3,1,4,2]]
     [[2,4,3,1],[4,1,3,2]]
     [[3,1,2,4],[2,3,1,4]]
     [[3,1,4,2],[2,4,1,3]]
     [[3,2,1,4],[3,2,1,4]]
     [[3,2,4,1],[4,2,1,3]]
     [[3,4,1,2],[3,4,1,2]]
     [[3,4,2,1],[4,3,1,2]]
     [[4,1,2,3],[2,3,4,1]]
     [[4,1,3,2],[2,4,3,1]]
     [[4,2,1,3],[3,2,4,1]]
     [[4,2,3,1],[4,2,3,1]]
     [[4,3,1,2],[3,4,2,1]]
     [[4,3,2,1],[4,3,2,1]]

  

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


%%
%% inverse/1
%%
%% For each element X[I] either:
%%     - X[I] = I
%%     or
%%     - X[I] = J <=> X[J] = I
%%
go :-
        N = 4,
        length(X,N),
        X ins 1..N,
        findall(X,(inverse(X),label(X)),L),
        length(L,Len),
        maplist(writeln,L),
        writeln(len=Len),
        nl.

%%
%% inverse/2
%%
go2 :-
        N = 4,
        length(X,N),
        X ins 1..N,
        length(Y,N),
        Y ins 1..N,
        findall([X,Y],(inverse(X,Y),append(X,Y,Vars), label(Vars)),L),
        length(L,Len),
        maplist(writeln,L),
        writeln(len=Len),
        nl.


%%
%% Note: The implementation of inverse/1 and inverse/2 has moved to
%%       hakank_utils.pl
%%

% %%
% %% inverse/1
% %%
% inverse(L) :-
%         inverse(L,L).

% %%
% %% inverse(L1,L2)
% %%
% %% For each element in L1 and L2
% %%     - L1[I] = I
% %%     or
% %%     - X[I] = J <=> X[J] = I
% %% 
% inverse(L1,L2) :-
%         %% same length
%         length(L1,Len),
%         length(L2,Len),
%         findall([I,J],(between(1,Len,I),between(1,Len,J)),IJs),
%         inverse_(IJs,L1,L2).


% inverse_([],_L1,_L2).
% inverse_([[I,J]|IJs],L1,L2) :-
%         element(I,L1,L1I),
%         element(J,L2,L2J),
%         (J #= L1I) #<==> (I #= L2J),
%         inverse_(IJs,L1,L2).

