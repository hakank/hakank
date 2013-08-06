/*

  Diet problem in B-Prolog.

  Standard diet problem.

  Minimize the cost for the products:
   Type of                        Calories   Chocolate    Sugar    Fat
   Food                                      (ounces)     (ounces) (ounces)
   Chocolate Cake (1 slice)       400           3            2      2
   Chocolate ice cream (1 scoop)  200           2            2      4
   Cola (1 bottle)                150           0            4      1
   Pineapple cheesecake (1 piece) 500           0            4      5


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        data(Price, Limits, Calories, Chocolate, Sugar, Fat),
        diet(Calories,Chocolate,Sugar,Fat,Price,Limits, Xs,XSum),
        writeln([cost:XSum, Xs]),

        % and get all optimal solutions
        % (It's a unique solution.)
        format('Find all solutions with cost ~d:\n', [XSum]),
        findall(Xs2,diet(Calories,Chocolate,Sugar,Fat,Price,Limits, Xs2,XSum),Sols),
        writeln(Sols),
        nl.


go2 :-
        data(Price, Limits, Calories, Chocolate, Sugar, Fat),       
        diet2([Calories,Chocolate,Sugar,Fat],Price,Limits, Xs,XSum),
        writeln([cost:XSum, Xs]).



diet(Calories,Chocolate,Sugar,Fat,Price,Limits, Xs,XSum) :-
        length(Price, Len), % for setting the length of Xs
        length(Xs, Len),
 	Xs :: 0..10,

        scalar_product(Calories,  Xs, #>=, Limits[1]), % 500,
        scalar_product(Chocolate, Xs, #>=, Limits[2]), %   6,
        scalar_product(Sugar,     Xs, #>=, Limits[3]), %  10,
        scalar_product(Fat,       Xs, #>=, Limits[4]), %   8,

        scalar_product(Price, Xs, #=, XSum), % to minimize

        % optimize or find all (optimal) solutions
        (
            var(XSum) -> 
                minof(labeling(Xs),XSum)
        ;
                % here XSum is bound so we just label the vars
                labeling(Xs)
        ).


%
% This is a more general solution where all the nutritions 
% are collected as a matrix.
%
diet2(Products,Price,Limits, Xs,XSum) :-
        length(Price, Len),
        length(Xs, Len),
 	Xs :: 0..10,

        foreach(P in 1..Products^length,
                [Product],
                (
                    Product @= Products[P],
                    scalar_product(Product, Xs,#>=, Limits[P])
                )
               ),
        scalar_product(Price, Xs, #=, XSum), % to minimize
        (
            var(XSum) -> 
                minof(labeling(Xs),XSum)
        ;
                labeling(Xs)
        ).


%
% data
%
data(
     [ 50, 20, 30, 80], % price in cents for each nutrition
     [500,  6, 10,  8], % limits, requirements for each nutrition type

     % nutrition for each product
     [400, 200, 150, 500], % calories
     [  3,   2,   0,   0],            % chocolate
     [  2,   2,   4,   4],            % sugar
     [  2,   4,   1,   5]             % fat
).
