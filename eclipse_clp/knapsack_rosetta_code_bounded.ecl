/*

  Knapsack (Bounded) in ECLiPSe.

  From 
  http://rosettacode.org/wiki/Knapsack_problem/Bounded
  """
   A tourist wants to make a good trip at the weekend with his 
   friends. They will go to the mountains to see the wonders of 
   nature. So he needs some items during the trip. Food, clothing, 
   etc. He has a good knapsack for carrying the things, but he knows 
   that he can carry only 4 kg weight in his knapsack, because they 
   will make the trip from morning to evening. He creates a list of 
   what he wants to bring for the trip, but the total weight of all 
   items is too much. He adds a value to each item. The value represents 
   how important the thing for the tourist. The list contains which 
   items are the wanted things for the trip, what is the weight and 
   value of an item, and how many units does he have from each items.
  
   This is the list:
   Table of potential knapsack items item 	weight (dag) (each) 	value (each) 	piece(s)
   map 	9 	150 	1
   compass 	13 	35 	1
   water 	153 	200 	2
   sandwich 	50 	60 	2
   glucose 	15 	60 	2
   tin 	68 	45 	3
   banana 	27 	60 	3
   apple 	39 	40 	3
   cheese 	23 	30 	1
   beer 	52 	10 	3
   suntan cream 	11 	70 	1
   camera 	32 	30 	1
   T-shirt 	24 	15 	2
   trousers 	48 	10 	2
   umbrella 	73 	40 	1
   waterproof trousers 	42 	70 	1
   waterproof overclothes 	43 	75 	1
   note-case 	22 	80 	1
   sunglasses 	7 	20 	1
   towel 	18 	12 	2
   socks 	4 	50 	1
   book 	30 	10 	2
   knapsack 	<=400 dag 	 ? 	 ? 
  
  
   The tourist can choose to take any combination of items from the 
   list, and some number of each item is available (see the column 
   "Piece(s)" of the list!). He may not cut the items, so he can only 
   take whole units of any item.
   
   Which items does the tourist carry in his knapsack so that their 
   total weight does not exceed 4 kg, and their total value is maximised?
  

  """

  This is an integer version of 
  http://www.hakank.org/eclipse/knapsack_rosetta_code_unbounded.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).

% total_value: 1010
% total_weight: 396
% map: 1
% compass: 1
% water: 1
% glucose: 2
% banana: 3
% cheese: 1
% suntancream: 1
% w-overclothes: 1
% note-case: 1
% sunglasses: 1
% socks: 1

go :-
        items(Items),
        length(Items,Rows),
        ( foreach([Item, Weight, Value, Piece],Items),
          foreach(Weight,Weights),
          foreach(Value,Values),
          foreach(Piece,Pieces),
          foreach(Item, AllItems) do
              true
        ),

        WeightLimit = 400,

        % 
        % Variables
        % 
        MaxPieces is max(Pieces),
        length(X,Rows),
        X :: 0..MaxPieces,

        %
        % Constraints
        % 
        SumValues is sum(Values),
        TotalValue :: 0..SumValues,
        TotalWeight :: 0..WeightLimit,

        scalar_product_list(X,Weights,TotalWeight),
        scalar_product_list(X,Values,TotalValue),
        TotalValueNeg #= -TotalValue,

        % check number of pieces
        ( foreach(XX,X),
          foreach(Piece, Pieces) do
              XX #=< Piece
        ),

        %
        % Search
        %
        minimize(search(X,0,first_fail,indomain_max,complete,
                        [backtrack(Backtracks)]), TotalValueNeg),

        %
        % Solutions
        % 
        writeln(backtracks:Backtracks),
        writeln('\nThese are the items to pick:'),
        writeln('  Item                    Weight Value'),
        ( foreach(Pick,X),
          foreach(Item,AllItems),
          foreach(Weight, Weights),
          foreach(Value, Values) do
              Pick #> 0 ->
              printf("* %d %-25s %4d %3d\n", [Pick, Item,Weight, Value])
        ;
              true
        ),
        nl,
        printf('Total weight: %d\n', [TotalWeight]),
        printf('Total value: %d\n', [TotalValue]).



scalar_product_list(List1,List2,Result) :-
        ( foreach(L1,List1),
          foreach(L2,List2),
          fromto(0,In,Out,Result) do
              Out #= L1*L2+In
        ).


       % Item                    Weight   Value  Pieces
items([["map",                     9,       150,   1],
       ["compass",                 13,      35,    1],
       ["water",                   153,     200,   2],
       ["sandwich",                50,      60,    2],
       ["glucose",                 15,      60,    2],
       ["tin",                     68,      45,    3],
       ["banana",                  27,      60,    3],
       ["apple",                   39,      40,    3],
       ["cheese",                  23,      30,    1],
       ["beer",                    52,      10,    3],
       ["suntancream",             11,      70,    1],
       ["camera",                  32,      30,    1],
       ["T-shirt",                 24,      15,    2],
       ["trousers",                48,      10,    2],
       ["umbrella",                73,      40,    1],
       ["waterproof trousers",     42,      70,    1],
       ["waterproof overclothes",  43,      75,    1],
       ["note-case",               22,      80,    1],
       ["sunglasses",              7,       20,    1],
       ["towel",                   18,      12,    2],
       ["socks",                   4,       50,    1],
       ["book",                    30,      10,    2]]).
