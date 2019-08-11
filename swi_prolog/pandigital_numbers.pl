/*

  Pandigital numbers in SWI Prolog

  From
  Albert H. Beiler "Recreations in the Theory of Numbers", quoted from
  http://www.worldofnumbers.com/ninedig1.htm
  """
  [ Chapter VIII : Digits - and the magic of 9 ]
  [ I found the same exposé in Shakuntala Devi's book
    "Figuring : The Joy of Numbers" ]

  The following curious table shows how to arrange the 9 digits so that
  the product of 2 groups is equal to a number represented by the
  remaining digits."

    12 x 483 = 5796
    42 x 138 = 5796
    18 x 297 = 5346
    27 x 198 = 5346
    39 x 186 = 7254
    48 x 159 = 7632
    28 x 157 = 4396
    4 x 1738 = 6952
    4 x 1963 = 7852
  """

  See also

  * MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
  """
  A number is said to be pandigital if it contains each of the digits
  from 0 to 9 (and whose leading digit must be nonzero). However,
  "zeroless" pandigital quantities contain the digits 1 through 9.
  Sometimes exclusivity is also required so that each digit is
  restricted to appear exactly once.
  """

  * Wikipedia http://en.wikipedia.org/wiki/Pandigital_number



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        findall([X1,X2,X3], pandigital([X1,X2,X3]),L),
        length(L,Len),
        writeln(len=Len).

%%
%% X1 * X2 #= X3
%%
pandigital([X1,X2,X3]) :-
        
        %
        % length of numbers
        %
        Len1 in 1..2,
        Len2 in 3..4,
        Len3 #= 4,

        Len1 #=< Len2, % symmetry breaking
        Len1 + Len2 + Len3 #= 9,
        
        indomain(Len1),

        % set length of lists
        length(X1,Len1),
        X1 ins 1..9,

        length(X2,Len2),
        X2 ins 1..9,

        length(X3,Len3), % the result
        X3 ins 1..9,

        % convert to number
        Base #= 10,
        to_num(X1, Base, Num1),
        to_num(X2, Base, Num2),
        to_num(X3, Base, Res),

        % calculate result
        Num1 * Num2 #= Res,

        flatten([X1,X2,X3],Vars),
        all_different(Vars),

        % search
        label(Vars),

        format("~t~d~5| * ~t~d~12| = ~t~d~19|\n",[Num1,Num2,Res]).
