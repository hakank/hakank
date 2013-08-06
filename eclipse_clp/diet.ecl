/*
   Diet problem in ECLiPSe.
   
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


   This ECLiPSe model was created by Hakan Kjellerstrand, hakank@bonetmail.com
   See also my ECLiPSe page: http://www.hakank.org/eclipse/
  
*/

:- lib(ic).
:- lib(branch_and_bound).

go :-
        data(Price, Limits, Calories, Chocolate, Sugar, Fat),
        writeln("limits    ":Limits),
        writeln("price    ":Price),
        writeln("calories ":Calories),
        writeln("chocolate":Chocolate),
        writeln("sugar    ":Sugar),
        writeln("fat      ":Fat),
        length(Calories, Len), % for setting the length of Xs
        length(Xs, Len),
 	Xs :: 0..10,
        
        % convert Limits to LimitArray
        length(Limits, LimitLength),
        dim(LimitArray, [LimitLength]),
        collection_to_list(LimitArray, Limits),

        % these constraints calculates: sum(Xs[i]*Y[i]) >= limit 
        Xs * Calories  #>= LimitArray[1], % 500,
        Xs * Chocolate #>= LimitArray[2], %   6,
        Xs * Sugar     #>= LimitArray[3], %  10,
        Xs * Fat       #>= LimitArray[4], %   8,

        XSum #= Xs * Price, % to minimize
        minimize(search(Xs,0,first_fail,indomain,complete,[]),XSum),

        writeln([XSum, Xs]).

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



