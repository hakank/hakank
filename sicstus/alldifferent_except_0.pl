/*

  Global constraint (decomposition) all different except 0 in ECLiPSe.

  Decomposition of the global constraint alldifferent except 0.
  
  From Global constraint catalogue:
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
  """ 
  Enforce all variables of the collection VARIABLES to take distinct values, except those 
  variables that are assigned to 0.
 
  Example
    (<5, 0, 1, 9, 0, 3>)
 
  The alldifferent_except_0 constraint holds since all the values (that are different from 0) 
  5, 1, 9 and 3 are distinct.
  """

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


%
% just a silly decomposition of alldifferent
%
alldifferent_decomp(Xs) :-
        ( foreach(XI, Xs), count(I,1,_),
          param(Xs) do
              ( foreach(XJ,Xs), count(J,1,_),
                param(XI,I) do
                    I < J
              ->
                XI #\= XJ
              ; 
                true
              )
        ),
        labeling([],Xs).



%
% alldifferent_except_0(Xs):
%
% All elements in Xs != 0 must be pairwise different.
%
% Note: The drawback of this version is that labeling
%       must be done before the loops, otherwise
%       it just rejects all lists containing 0. 
%       I'm not sure how usable it is...
%
alldifferent_except_0(Xs) :-
        ( foreach(X,Xs) do indomain(X)),
        ( foreach(XI,Xs), count(I,1,_), 
          param(Xs) 
        do
          ( foreach(XJ,Xs), count(J,1,_),
            param(I,XI) do 
                I < J, XI #\=0, XJ #\=0  
          ->
            XI #\= XJ
          ; 
            true
          )
        ).


% This is an earlier version, with excessive use of element/3.
% alldifferent_except_0_with_element(Xs) :-
%         length(Xs, Len),
%         labeling([],Xs),
%         ( for(I,1,Len), param(Xs, Len) do
%               (
%                   for(J,1,Len), param(Xs,I) do 
%                       (I \= J,
%                        element(I,Xs,XI),
%                        XI #\=0, 
%                        element(J,Xs,XJ),
%                        XJ #\=0)  ->
%                       XI #\= XJ
%               ; 
%                       true
%               )
%         ).


go :-
        Len = 3,
        length(Ys, Len),
        domain(Ys, 0,4),
        findall(Ys,  alldifferent_except_0(Ys), L),
        length(L, LLen),
        (foreach(X, L) do
             write(X),nl
        ),
        format('It was ~d solutions',LLen),nl,nl,
        fd_statistics.


go2 :-
        Xs = [5, 0, 1, 9, 0, 3],
        alldifferent_except_0(Xs),
        write(Xs), nl.
