/*

  Diet problem in SWI Prolog

  Standard diet problem.

  Minimize the cost for the products:
   Type of                        Calories   Chocolate    Sugar    Fat
   Food                                      (ounces)     (ounces) (ounces)
   Chocolate Cake (1 slice)       400           3            2      2
   Chocolate ice cream (1 scoop)  200           2            2      4
   Cola (1 bottle)                150           0            4      1
   Pineapple cheesecake (1 piece) 500           0            4      5



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

% find all optimal solutions
go :-
        data(Price, Limits, [Calories, Chocolate, Sugar, Fat]),
        once(diet(Calories,Chocolate,Sugar,Fat,Price,Limits, Xs,XSum)),
        writeln([cost=XSum, Xs]),

        % and get all optimal solutions
        % (It happens to be a unique solution.)
        format("Find all solutions with cost ~d:\n", XSum),
        findall(Xs2,diet(Calories,Chocolate,Sugar,Fat,Price,Limits, Xs2,XSum),Sols),
        writeln(Sols),
        nl.


go2 :-
        data(Price, Limits, [Calories, Chocolate, Sugar, Fat]),       
        diet2([Calories,Chocolate,Sugar,Fat],Price,Limits, Xs,XSum),
        writeln([cost=XSum, Xs]).


% 
% Diet problem
%
diet(Calories,Chocolate,Sugar,Fat,Price,Limits, Xs,XSum) :-

        length(Price,Len),
        length(Xs, Len),
 	Xs ins 0..10,

        nth1(1,Limits,Limits1),
        nth1(2,Limits,Limits2),
        nth1(3,Limits,Limits3),
        nth1(4,Limits,Limits4),
        
        scalar_product(Calories,  Xs, #>=, Limits1), % 500,
        scalar_product(Chocolate, Xs, #>=, Limits2), %   6,
        scalar_product(Sugar,     Xs, #>=, Limits3), %  10,
        scalar_product(Fat,       Xs, #>=, Limits4), %   8,
        
        scalar_product(Price, Xs, #=, XSum), % to minimize
        
        % optimize or find all (optimal) solutions
        ( var(XSum)
        -> 
          labeling([min(XSum)], Xs)
        ; 
          % here XSum is bound so we just label the vars
          labeling([], Xs)
        ).

%
% This is a more general solution where all the nutritions 
% are handled in a foreach loop.
%
diet2(Products,Price,Limits, Xs,XSum) :-

        length(Price,Len),
        length(Xs,Len),
        Xs ins 0..10,

        scalar_product_rows(Products,Xs,#>=,Limits),
        scalar_product(Price, Xs, #=, XSum), % to minimize
        (var(XSum)
        ->
         labeling([min(XSum)], Xs)
        ; 
         labeling([],Xs)
        ).

scalar_product_rows([],_Xs,_Rel,_Limits).
scalar_product_rows([Product|Products],Xs,Rel,[Limit|Limits]) :-
        scalar_product(Product, Xs,Rel, Limit),
        scalar_product_rows(Products,Xs,Rel,Limits).

%
% data
%
data(Price, Limits, Nutrition) :-
        Price = [ 50, 20, 30, 80], % price in cents for each nutrition
        Limits = [500,  6, 10,  8], % limits, requirements for each nutrition type
        
        % nutrition for each product
        Nutrition = 
        [[400, 200, 150, 500],  % calories
         [  3,   2,   0,   0],  % chocolate
         [  2,   2,   4,   4],  % sugar
         [  2,   4,   1,   5]]. % fat


