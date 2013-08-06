/*

  Set covering problem in B-Prolog.

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  AMPL model: http://taha.ineg.uark.edu/setcover.txt


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



%
% First find the optimal value (MinVal) i.e. the number of telephones
% placed. Then find all the solutions with that value.
%
go :-
        writeln('Find the optimal solution:'),
        corners(N, Corners),
        set_covering2(N, Corners, MinVal,_),

        format("\nFinding all optimal solutions with MinVal ~d:\n", [MinVal]),
        findall(X, set_covering2(N, Corners,MinVal,X), L),
        length(L, Len),
        writeln(L),
        format("It was ~d solutions\n", [Len]).


set_covering2(N, Corners, MinVal, X) :-
      
        % where to place the telephones
        length(X,N),
        X :: 0..1,
        
        NumStreets @= Corners^length,

        % All streets must be covered
        foreach(I in 1..NumStreets, 
                X[Corners[I,1]] + X[Corners[I,2]] #>= 1),


        % objective: minimize the number of telephones
        MinVal #= sum(X),

        % Either search for all solutions (with the minimum value) or
        % search for the optimal value.
        (
            ground(MinVal) 
        -> 
            labeling(X)
        ;
            minof(labeling(X),MinVal)
        ),

        writeln(minVal:MinVal),
        writeln(x:X).


% corners of each street
%
% corners(NumberOfStreets, Corners)
%
corners(8, []([](1,2),
              [](2,3),
              [](4,5),
              [](7,8),
              [](6,7),
              [](2,6),
              [](1,6),
              [](4,7),
              [](2,4),
              [](5,8),
              [](3,5))).

