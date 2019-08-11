/*

  Set covering problem in SWI Prolog

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  AMPL model: http://taha.ineg.uark.edu/setcover.txt

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% First find the optimal value (MinVal) i.e. the number of telephones
% placed. Then find all the solutions with that value.
%
go :-
        writef("Find the optimal solution:\n"),
        corners(N, Corners),

        set_covering2(N, Corners, MinVal, _X),
        
        format("\nFinding all optimal solutions with MinVal ~d:\n", [MinVal]),
        findall(X, set_covering2(N, Corners, MinVal, X),L),
        writeln(L),        
        length(L,Len),
        format("It was ~d solutions\n", Len).


set_covering2(N, Corners, MinVal, X) :-

        %% where to place the telephones
        length(X,N),
        X ins 0..1,
        
        % All streets must be covered
        maplist(cover_corners(X),Corners),

        % objective: minimize the number of telephones
        sum(X,#=,MinVal),

        % Either search for all solutions (with the minimum value) or
        % search for the optimal value.
        (ground(MinVal) ->
         label(X)
        ; 
         labeling([min(MinVal)], X)
        ),

        writeln(minVal=MinVal),
        writeln(x=X).

cover_corners(X,[I,J]) :-
        element(I,X,XI),
        element(J,X,XJ),
        XI + XJ #>= 1.


%
% corners of each street
%
% corners(NumberOfStreets, Corners)
%
corners(N, Corners) :- 
        N = 8,
        Corners =
        [[1,2],
         [2,3],
         [4,5],
         [7,8],
         [6,7],
         [2,6],
         [1,6],
         [4,7],
         [2,4],
         [5,8],
         [3,5]].

