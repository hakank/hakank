/*

  Strimko puzzle in SWI Prolog

  From 
  360: A New Twist on Latin Squares
  http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
  """
  The idea is simple: each row and column of an nxn grid must contain 
  the number 1, 2, ... n exactly once (that is, the grid must form a 
  Latin square), and each "stream" (connected path in the grid) must 
  also contain the numbers 1, 2, ..., n exactly once.
  """
 
  For more information, see:
  * http://www.strimko.com/
  * http://www.strimko.com/rules.htm
  * http://www.strimko.com/about.htm
  * http://www.puzzlersparadise.com/Strimko.htm

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        strimko(2).

go2 :-
        Problems = [2,67,68,69,70],
        member(P,Problems),
        strimko(P),
        fail,
        nl.

go2.


strimko(P) :-
        format("\nProblem ~w\n",P),
        problem(P,Streams,Placed),

        length(Streams,N),
        writeln(n=N),
        new_matrix(N,N,1..N,X),
        flatten(X,Vars),
        
        % is a latin square
        latin_square(X),

        % streams
        % To simplify it, both X and Streams are flattened.
        flatten(Streams,StreamsFlatten),
        numlist(1,N,Ss),
        maplist(streams(Vars,StreamsFlatten), Ss),

        % placed numbers
        maplist(placed(X),Placed),
        
        labeling([ff,bisect],Vars),
        pretty_print(X),
        nl.

streams(X,Streams,S) :-
        length(X,Len),
        findall(I,(between(1,Len,I),
                   element(I,Streams,S)
                  ),Is),
        extract_from_indices(Is,X,XI),
        all_different(XI).
        
placed(X, [P1,P2,P3]) :-
        matrix_element(X,P1,P2,P3).
               

pretty_print(X) :-
        maplist(writeln,X).



%
% Strimko Monthly #02
% Via http://www.hakank.org/minizinc/strimko2_002.dzn
problem(2,Streams, Placed) :-
        Streams = [[1,1,2,2,2,2,2],
                   [1,1,2,3,3,3,2],
                   [1,4,1,3,3,5,5],
                   [4,4,3,1,3,5,5],
                   [4,6,6,6,7,7,5],
                   [6,4,6,4,5,5,7],
                   [6,6,4,7,7,7,7]],
        Placed =  [[2,1,1],
                   [2,3,7],
                   [2,5,6],
                   [2,7,4],
                   [3,2,7],
                   [3,6,1],
                   [4,1,4],
                   [4,7,5],
                   [5,2,2],
                   [5,6,6]].

% 
% Strimko Weekly Set 067
% Via http://www.hakank.org/minizinc/strimko2_067.dzn
problem(67,Streams, Placed) :-
        Streams =  [[1,1,1,2,3],
                    [1,2,2,2,3],
                    [1,2,4,5,3],
                    [5,4,5,4,3],
                    [4,5,5,4,3
                    ]],
        Placed =  [[1,3,4],
                   [1,4,1],
                   [3,3,2],
                   [3,5,3],
                   [5,4,5]].

%
% Strimko Weekly Set 068
% Via http://www.hakank.org/minizinc/strimko2_068.dzn
problem(68,Streams,Placed) :-
        Streams = [[1,2,2,4],
                   [2,1,4,2],
                   [3,4,1,3],
                   [4,3,3,1]],
        Placed =  [[2,2,3],
                   [2,3,2],
                   [3,3,1]].


% Strimko Weekly Set 069
% Via http://www.hakank.org/minizinc/strimko2_069.dzn
problem(69,Streams,Placed) :-
        Streams = [[1,2,3,3,3,4],
                   [2,1,3,5,4,3],
                   [2,1,3,5,5,4],
                   [2,6,1,6,5,4],
                   [2,6,1,6,4,5],
                   [6,2,6,1,5,4]],
        Placed =  [[2,2,4],
                   [2,3,1],
                   [2,4,3],
                   [2,5,2],
                   [3,2,1],
                   [3,5,6],
                   [4,3,5],
                   [4,4,2]].

% Strimko Weekly Set 070
% Via http://www.hakank.org/minizinc/strimko2_070.dzn
problem(70,Streams,Placed) :-
        Streams =  [[1,2,3,3,3],
                    [2,1,1,3,1],
                    [2,2,3,1,4],
                    [5,2,5,4,4],
                    [5,5,5,4,4]],
        Placed =   [[1,1,1],
                    [2,5,4],
                    [4,1,2],
                    [5,4,5]].
