/*

  Pandigital numbers  in SICStus Prolog.

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

  * Also see my other pandigital number models
    - MiniZinc: http://www.hakank.org/minizinc/pandigital_numbers.mzn
    - Gecode/R: http://www.hakank.org/gecode_r/pandigital_numbers.rb
    - Comet   : http://www.hakank.org/comet/pandigital_numbers.co
    - ECLiPSe : http://www.hakank.org/eclipse/pandigital_numbers.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


%
% converts a number Num to/from a list of integer List given a base Base
%
toNum2(List, Base, Num) :-
        length(List, Len),      
        length(Xs, Len),
        exp_list2(Len, Base, Xs),
        scalar_product(Xs,List,#=, Num).

%
% Exponents for toNum2: [Base^(N-1), Base^(N-2), .., Base^0],
%    e.g. exp_list2(3, 10, ExpList) -> ExpList = [100,10,1]
%
exp_list2(N, Base, ExpList) :-
        length(ExpList, N),
        ( for(I, 0, N-1), 
          fromto([], In, Out, ExpList), 
          param(Base)  do 
            B is integer(Base**I),
            Out = [B|In]
        ).


pandigital([X1,X2,X3]) :-
        
        % length of numbers
        Len1 #>= 1,
        Len1 #=< 4,
        Len2 #>= 1,
        Len2 #=< 4,
        Len3 #= 4,

        Len1 #=< Len2, % symmetry breaking
        Len1 + Len2 + Len3 #= 9,

        indomain(Len1), % Thanks to Mats Carlsson for suggesting this.

        % set length of lists
        length(X1, Len1),
        length(X2, Len2),
        length(X3, Len3), % the result

        append([X1,X2,X3], Vars),
        domain(Vars,1,9),

        % convert to number
        Base = 10,

        toNum2(X1, Base, Num1),
        toNum2(X2, Base, Num2),
        toNum2(X3, Base, Res),

        % calculate result
        Num1 * Num2 #= Res,

        all_distinct(Vars),

        % search
        labeling([ff,bisect,down], Vars),

        format('~d * ~d = ~d: ', [Num1,Num2, Res]),
        write([X1,X2,X3]),nl.
        % fd_statistics.

go :-
        findall([X1,X2,X3], pandigital([X1,X2,X3]), L),
        % pandigital(_),
        length(L, Len),
        write(L),nl,
        write(len:Len),nl.
