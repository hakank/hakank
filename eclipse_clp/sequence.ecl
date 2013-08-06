/*

  (Decomposition of) global constraint sequence in ECLiPSe.


  From the MiniZinc definition:
  """
  Requires that in each subsequence 'x[i], ..., x[i + l - 1]' the sum of the
  variables is between 'mn' and 'mx'.
      sequence(array[int] of var int: x, int: l, int: mn, int: mx)
  """

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).

%
% sequence(?X,+Length,?LBound,?UBound)
% 
% Ensures that all sums of every subsequence of length Length
% in array X is between LBound and UBound
% 
sequence(X,Length, LBound,UBound) :-
        dim(X,[XLen]),
        ( for(I,1,XLen-Length+1), 
          param(X,Length,LBound,UBound) do
              ( for(J,I,I+Length-1),param(X),
                fromto(0,In,In+X[J],ThisSumTmp) do
                    true
              ),
              ThisSum #= eval(ThisSumTmp),
              ThisSum #>= LBound,
              ThisSum #=< UBound
        ).
        

go :-
        
        N = 11,
        dim(X, [N]),
        X :: 1..N,

        % Sum of X (just for show)
        ( for(I,1,N), 
          fromto(0,In,In+X[I],SSum),
          param(X) do
              true
        ),
        Sum #= eval(SSum),

        Length = 3,
        % Length may be a decision varible, but
        % sequence/4 must be wrapped within a suspend()
        % Length :: 2..4,

        LBound :: 1..N-1, % lower bound
        UBound :: 1..N-1, % upper bound         
        LBound #= UBound,

        sequence(X,Length,LBound,UBound),

        % if Length is a decision variable this is needed
        % otherwise an instantiation fault occurs
        % suspend(sequence(X,Length,LBound,UBound),2,Length->inst),

        term_variables([X,LBound,UBound, Length], Vars),

        labeling(Vars),
        

        writeln(x:X),
        writeln(sum:Sum),
        writeln(lower_bound:LBound),
        writeln(upper_bound:UBound),
        writeln(length:Length),
        nl,
        fail.


