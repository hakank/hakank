/*

  (Decomposition of) global constraint sequence in SICStus Prolog.

  From the MiniZinc definition:
  """
  Requires that in each subsequence 'x[i], ..., x[i + l - 1]' the sum of the
  variables is between 'mn' and 'mx'.
      sequence(array[int] of var int: x, int: l, int: mn, int: mx)
  """

  Compare with the following model: 
  * ECLiPSe: http://www.hakank.org/eclipse/sequence.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% sequence(?X,+Length,?LBound,?UBound)
% 
% Ensures that all sums of every subsequence of length Length
% in array X is between LBound and UBound
% 
sequence(X,Length, LBound,UBound) :-
        length(X,XLen),
        ( for(I,1,XLen-Length+1), 
          param(X,Length,LBound,UBound) do
              ( for(J,I,I+Length-1),
                param(X),
                fromto(0,In,Out,ThisSum) do
                    element(J,X,XJ),
                    Out #= In + XJ
              ),
              ThisSum #>= LBound,
              ThisSum #=< UBound
        ).


go :-
        N = 11,
        length(X, N),
        domain(X,1,N),

        % Sum of X (just for show)
        sum(X,#=,Sum),

        % Length may be a decision varible
        % but must be fixed
        Length in 2..4,
        Length #= 3,

        N1 is N-1,
        LBound in 1..N1, % lower bound
        UBound in 1..N1, % upper bound         
        LBound #= UBound,

        sequence(X,Length,LBound,UBound),

        % term_variables([X,LBound,UBound, Length], Vars),
        append(X,[LBound,UBound,Length], Vars),

        labeling([], Vars),
        
        write(x:X),nl,
        write(sum:Sum),nl,
        write(lower_bound:LBound),nl,
        write(upper_bound:UBound),nl,
        write(length:Length),nl,
        nl,
        fail.


