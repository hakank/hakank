/*

  (Decomposition of) global constraint sequence in B-Prolog.

  From the MiniZinc definition:
  """
  Requires that in each subsequence 'x[i], ..., x[i + l - 1]' the sum of the
  variables is between 'mn' and 'mx'.
      sequence(array[int] of var int: x, int: l, int: mn, int: mx)
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N = 11,
        length(X, N),
        X :: 1..N,

        % Sum of X (just for show)
        Sum #= sum(X),

        % Length may be a decision varible,
        % but must be fixed
        Length in 2..4,
        Length #= 3,

        N1 is N-1,
        LBound :: 1..N1, % lower bound
        UBound :: 1..N1, % upper bound         
        LBound #= UBound,
        % LBound #=< UBound,

        sequence(X,Length,LBound,UBound),

        term_variables([X,LBound,UBound, Length], Vars),

        labeling([], Vars),
        
        writeln(x:X),
        writeln(sum:Sum),
        writeln(lower_bound:LBound),
        writeln(upper_bound:UBound),
        writeln(length:Length),
        nl,
        fail.



%
% sequence(?X,+Length,?LBound,?UBound)
% 
% Ensures that all sums of every subsequence of length Length
% in array X is between LBound and UBound
% 
sequence(X,Length, LBound,UBound) :-
        length(X,XLen),
        foreach(I in 1..XLen-Length+1, 
                [Sum],
                (Sum #= sum([XJ : J in I..I+Length-1, [XJ], element(J,X,XJ)]),
                 Sum #>= LBound,
                 Sum #=< UBound
                )
        ).


