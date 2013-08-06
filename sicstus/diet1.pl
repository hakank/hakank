/*
   Diet problem in SICStus Prolog.
   
   Standard diet problem.

   Minimize the cost for the products:
   Type of                        Calories   Chocolate    Sugar    Fat
   Food                                      (ounces)     (ounces) (ounces)
   Chocolate Cake (1 slice)       400           3            2      2
   Chocolate ice cream (1 scoop)  200           2            2      4
   Cola (1 bottle)                150           0            4      1
   Pineapple cheesecake (1 piece) 500           0            4      5
   
    
   Compare with the following models:
   * MiniZinc: http://www.hakank.org/minizinc/diet1.mzn
   * Choco   : http://www.hakank.org/choco/Diet.java
   * JaCoP   : http://www.hakank.org/JaCoP/Diet.java
   * Gecode/R: http://www.hakank.org/gecode_r/diet.rb
   * Comet   : http://www.hakank.org/comet/diet.co
   * Gecode  : http://www.hakank.org/gecode/diet.cpp
   * ECLiPSe : http://www.hakank.org/eclipse/diet.ecl

   This model was created by Hakan Kjellerstrand, hakank@bonetmail.com
   See also my SICStus Prolog page: http://www.hakank.org/sicstus/
  
*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

writeln(Str) :-
        write(Str), nl.

go :-
        data(Price, Limits, Calories, Chocolate, Sugar, Fat),
        writeln('limits   ':Limits),
        writeln('price    ':Price),
        writeln('calories ':Calories),
        writeln('chocolate':Chocolate),
        writeln('sugar    ':Sugar),
        writeln('fat      ':Fat),
        length(Calories, Len), % for setting the length of Xs
        length(Xs, Len),
 	domain(Xs, 0, 10),
        
        [CaloriesLimit,ChocolateLimit, SugarLimit, FatLimit] = Limits,

        % note:
        % scalar_product(Coeff, CoffsOrVars, Op, IntOrVar)
        % 
        scalar_product(Calories, Xs, #>=, CaloriesLimit),
        scalar_product(Chocolate, Xs, #>=, ChocolateLimit),
        scalar_product(Sugar, Xs, #>=, SugarLimit),
        scalar_product(Fat, Xs, #>=, FatLimit),

        scalar_product(Price, Xs, #=, XSum),
        labeling([ff,bisect,down,minimize(XSum)],Xs),

        writeln([XSum, Xs]),
        fd_statistics.

%
% data
%
data(
        [ 50, 20, 30, 80], % price in cents for each nutrition
        [500,  6, 10,  8], % limits, requirements for each nutrition
                           % type

        % nutrition for each product
        [400, 200, 150, 500], % calories
        [3,2,0,0],            % chocolate
        [2,2,4,4],            % sugar
        [2,4,1,5]             % fat
).
